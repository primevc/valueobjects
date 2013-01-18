;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.util.elasticsearch
  (:refer-clojure :exclude [get])
  (:require [prime.vo        :as   vo]
            [clj-http.client :as http]
            [cheshire.core   :as json], cheshire.generate, cheshire.custom, cheshire.factory
            [clj-elasticsearch.client :as ces])
  (:use [prime.vo.source :only [def-valuesource]])
  (:import [prime.types VORef EnumValue package$ValueType package$ValueTypes$Tdef package$ValueTypes$Tarray package$ValueTypes$Tenum]
           [prime.vo IDField ValueObject ValueObjectField ValueObjectCompanion ID]
           [com.fasterxml.jackson.core JsonGenerator]

           org.elasticsearch.action.search.SearchResponse, [org.elasticsearch.search SearchHit, SearchHitField],
           org.elasticsearch.action.get.GetResponse))

(set! *warn-on-reflection* true)

;(defonce es-client (ces/make-client :transport {:hosts ["127.0.0.1:9300"] :cluster-name (str "elasticsearch_" (System/getenv "USER"))}))

(defn create-client [hosts cluster-name & options]
  (ces/make-client :transport {:hosts hosts :cluster-name cluster-name}))



;
; Generate ElasticSearch mapping from VO:
;

(defn mapping-field-type-name
  [^package$ValueType valueType]
  (case (. valueType keyword)
    :prime.types/boolean    "boolean"
    :prime.types/integer    "integer"
    :prime.types/decimal    "double"
    :prime.types/Date       "date"
    :prime.types/Date+time  "date"
    :prime.types/Interval   "object"
    :prime.types/Color      "integer"
    :prime.types/String     "string"
    :prime.types/URI        "string"
    :prime.types/URL        "string"
    :prime.types/E-mail     "string"
    :prime.types/ObjectId   "string"
    :prime.types/FileRef    "string"
    #_default
      (condp instance? valueType
        package$ValueTypes$Tdef   "object"
        package$ValueTypes$Tenum  "object"
        package$ValueTypes$Tarray (assert false "array should be mapped as it's contents"))))

(declare vo-mapping)

(defn field-mapping
  ([option-map ^ValueObjectField field unique]
    (field-mapping (conj (if unique {:index "not_analyzed"} #_else {}) option-map)
                   (. field valueType) (. field id) (. field keyword)))

  ([option-map, ^package$ValueType value-type, id, field-key]
    (if (instance? package$ValueTypes$Tarray value-type)
      (field-mapping option-map (. ^package$ValueTypes$Tarray value-type innerType) id field-key)
    ; Else: not an array
    { (Integer/toHexString id),
    (conj {:store "no"
           :index_name (name field-key)
           :type       (mapping-field-type-name value-type)}
      (cond
        (instance? package$ValueTypes$Tenum value-type)
          {:properties {:v {:type "integer"} :x {:type "string", :index "not_analyzed"}}} ;x = extended value, currently only String

        (instance? package$ValueTypes$Tdef  value-type)
          (let [^package$ValueTypes$Tdef value-type value-type
                ^ValueObject             empty      (.. value-type empty)]
            (conj
              (if (.. value-type ref)
                {:type (mapping-field-type-name (.valueType (._id ^IDField (. empty voManifest))))}
              #_else
                  (vo-mapping empty (or option-map {})))

              option-map
          ))

        (= :prime.types/Interval field-key)
          {:properties {:s {:type "date"} :e {:type "date"}}}

        :else nil)

      option-map ;overwrites defaults
    )
    })))


(defn vo-mapping "Create an ElasticSearch compatible mapping from an empty ValueObject.
  options are
  - :only    exclusive #{set} of fields to include in mapping
  - :exclude #{set} of fields not to include in mapping
  - any ElasticSearch option.

  Known issues / TODO:
   - ValueObjects fields need to have their subtypes mappings merged in,
      or ElasticSearch will complain about strict mapping when storing a vo that has subtype specific fields.
  "
  ([^ValueObject vo] (vo-mapping vo {}))

  ([^ValueObject vo, option-map]
   (let [id-field (if (instance? IDField (. vo voManifest)) (._id ^IDField (. vo voManifest)))]
    { :type       "object"
      :dynamic    "strict"
      :properties
      (into {}
        (map
          (fn [^ValueObjectField field] (field-mapping (option-map (.keyword field)) field (if id-field (identical? id-field field))))
          (vo/field-filtered-seq vo (:only option-map) (:exclude option-map))))
    }))
)

(defn ^String field-hexname [^ValueObjectField field]
  (Integer/toHexString (.id field)))

(defn keyword->hex-field   [^prime.vo.ValueObject vo, ^clojure.lang.Named key]
  (let [path (prime.vo/fields-path-seq vo (clojure.string/split (name key) #"[.]"))]
    (if path
      (clojure.string/join "."
        (map field-hexname path)))))

(defn map-keywords->hex-fields [^prime.vo.ValueObject vo, expr]
  (cond
    (instance? ValueObject expr)
      expr
    (keyword? expr)
      (or (keyword->hex-field vo expr) expr)
    (vector? expr) ; Clojure wart: A MapEntry is a vector, but not a PersistentCollection :-S and does not implement (empty ..)
      (vec (map (partial map-keywords->hex-fields vo) expr))
    (empty expr)
      (into (empty expr) (map (partial map-keywords->hex-fields vo) expr))
    :else expr))

(defn vo-hexname [^ValueObject vo] (Integer/toHexString (.. vo voManifest ID)))


;
; ElasticSearch index management (mapping API)
;

(defn vo-index-mapping-pair [[^ValueObject vo, options]]
  (assert options)
  [ (Integer/toHexString (.. vo voManifest ID)),
    (conj (dissoc (vo-mapping vo options) :type) options) ])

(defn put-mapping [es index-name, vo-options-pair]
  (let [[type mapping] (vo-index-mapping-pair vo-options-pair)]
    (ces/put-mapping es, {
      :index  index-name
      :type   type
      :source {type mapping}
    })))

(defn create-index
  ([es index-name vo->options-map]
    (create-index es index-name vo->options-map {}))
  ([es index-name vo->options-map root-options]
    (ces/create-index es, {
      :index  index-name
      :source (assoc root-options :mappings (into {} (map vo-index-mapping-pair vo->options-map)))
    })))


;
; ValueObjects and valuetypes JSON encoding
;

(defn encode-enum [^EnumValue in ^JsonGenerator out]
  (.writeStartObject out)
  (if (not (.isInstance scala.Product in))
    (.writeNumberField out "v", (.value in))
    #_else
    (.writeObjectField out "x", (.toString in)))
  (.writeEndObject out))

(defn encode-vo
  ([^JsonGenerator out, ^prime.vo.ValueObject vo ^String date-format ^Exception ex]
    (encode-vo out vo date-format ex (.. vo voManifest ID)))

  ([^JsonGenerator out, ^prime.vo.ValueObject vo ^String date-format ^Exception ex ^Integer baseTypeID]
    (.writeStartObject out)

    (if (not (== baseTypeID (.. vo voManifest ID)))
      (.writeNumberField out "t" (.. vo voManifest ID)))

    (doseq [[^ValueObjectField k v] vo]
      (.writeFieldName out (field-hexname k))
      (if (instance? ValueObject v)
        (encode-vo out v date-format ex (.. ^package$ValueTypes$Tdef (. k valueType) empty voManifest ID))
      #_else
        (cheshire.generate/generate out v date-format ex)))

    (.writeEndObject out)))

(defn encode-voref [^JsonGenerator out, ^VORef in ^String date-format ^Exception ex]
  (cheshire.generate/generate out (._id in) date-format ex))

(defn encode-instant [^org.joda.time.ReadableInstant in ^JsonGenerator out]
  (.writeNumber out (.getMillis in)))

(defn encode-uri [^java.net.URI in ^JsonGenerator out]
  (.writeString out (.toString in)))

(defn encode-url [^java.net.URL in ^JsonGenerator out]
  (.writeString out (.toString in)))

(defn encode-internetAddress [^javax.mail.internet.InternetAddress in ^JsonGenerator out]
  (.writeString out (.toString in)))

(defn encode-objectId [^org.bson.types.ObjectId in ^JsonGenerator out]
  (.writeString out (.toString in)))

(doseq [add-encoder [cheshire.generate/add-encoder, cheshire.custom/add-encoder]]
  (add-encoder prime.types.EnumValue                encode-enum)
  (add-encoder prime.vo.ValueObject                 encode-vo)
  (add-encoder java.net.URI                         encode-uri)
  (add-encoder java.net.URL                         encode-url)
  (add-encoder org.joda.time.ReadableInstant        encode-instant)
  (add-encoder org.bson.types.ObjectId              encode-objectId)
  (add-encoder javax.mail.internet.InternetAddress  encode-internetAddress))

(alter-var-root #'cheshire.generate/generate
  (fn [orig-generate]
    (fn [^JsonGenerator jg obj ^String date-format ^Exception ex]
      (binding [prime.vo/*voseq-key-fn* identity]
        (if (instance? ValueObject obj) (encode-vo     jg obj date-format ex)
        #_else(if (instance? VORef obj) (encode-voref  jg obj date-format ex)
        #_else                          (orig-generate jg obj date-format ex)))))))


;
; ElasticSearch ValueSource
;

(declare convert-search-result)

(def-valuesource ElasticSearch-ValueSource [^int type, ^java.util.Map jmap, response]
  (typeID   [this, base] (if (not (== -1 type)) type #_else base))

  (contains [this, name idx]          (.containsKey jmap (Integer/toHexString (bit-shift-right idx 8))))
  (anyAt    [this, name idx notFound] (or
    (if jmap
      (let [item (.get jmap (Integer/toHexString (bit-shift-right idx 8)))]
        (convert-search-result item)))
    notFound)))

(defn convert-search-result [item]
  (condp instance? item
  SearchHitField
    (convert-search-result (.value ^SearchHitField item))

  java.util.Map
    (let [item ^java.util.Map item]
      (if-let [x (.get item "x")] x
      (if-let [v (.get item "v")] v
      (ElasticSearch-ValueSource. (or (.get item "t") -1), item, nil))))

  java.util.List
    (map convert-search-result item)

  item))


;
; ElasticSearch querying API
;

(defn vo->term-filter
  ([vo]
    (let [terms (vo->term-filter vo "")]
      (if (== 1 (count terms))
        {:term terms}
      #_else
        {:and (map #(let [[k v] %1] {:term {k v}}) terms)})))

  ([vo, prefix]
    (binding [prime.vo/*voseq-key-fn* #(str prefix (field-hexname %))]
      (into {}
        (map (fn [pair] (let [[k v] pair]
              (cond
                (map? v)                (vo->term-filter v (str k "."))
                (vector? v)             (if (instance? ValueObject (first v)) (vo->term-filter (first v) (str k ".")) #_else [k (first v)])
                (instance? EnumValue v) (if (.isInstance scala.Product v) [(str k ".x") (.toString v)] #_else [(str k ".v") (.value ^EnumValue v)])
                :else  pair)))
        (seq vo))))))


(defn get
  "options: see clj-elasticsearch.client/get-doc"
  ([es es-index vo]
    (get es es-index vo {}))

  ([es es-index ^ValueObject vo options]
    ;{:pre [ ... ]} is bugged for multi arity fns
    (assert es) (assert (string? es-index)) (assert (not (empty? vo))) (assert options)
    (let [resp ^GetResponse
          (ces/get-doc es (assoc options, :index es-index, :format :java, :id (.. ^ID vo _id toString)))]
      (.apply (. vo voCompanion)
              (ElasticSearch-ValueSource. (.. vo voManifest ID) (.sourceAsMap resp) resp)))))

(defn put
  "options: see clj-elasticsearch.client/index-doc"
  [es ^ValueObject vo & {:as options :keys [index]}]
  (assert index ":index required")
  (assert (prime.vo/has-id? vo) (str "vo: " (prn-str vo) " requires an id"))
  (ces/index-doc es (conj options {
    :id     (prime.types/to-String (.. ^ID vo _id))
    :type   (Integer/toHexString (.. vo voManifest ID))
    :source (json/encode-smile vo)
  })))

;
; Search
;

(defn SearchHit->fields [^SearchHit sh] (.fields sh)     )
(defn SearchHit->source [^SearchHit sh] (.sourceAsMap sh))

(defn SearchHit->ValueObject [source-fn, ^ValueObjectCompanion voCompanion, ^SearchHit hit]
  (.apply voCompanion
    (ElasticSearch-ValueSource. (Integer/parseInt (.type hit) 16) (source-fn hit) hit)))

(defn scroll-seq [es, ^SearchResponse scroll-req keep-alive, from]
  (let [ ^SearchResponse response (ces/scroll es {:scroll-id (.scrollId scroll-req) :scroll keep-alive, :format :java})
          hits     (.. response hits hits)
          num-hits (.. response hits totalHits)
          last-hit (+  from (count hits)) ]
    (if (and (.scrollId response) (< last-hit num-hits))
      (concat hits (lazy-seq (scroll-seq es response keep-alive last-hit)))
    #_else    hits)))


; TODO:
; - add "type" filter by default.
; - use id->voCompanion when type filter is removed, to allow searching for multiple types.
(defn search
  "options: see clj-elasticsearch.client/search
  example: (vo/search {:filter {:term {:name 'Henk'}}})

  Set indices to [\"*\"] to search in all.

  Returns: seq of SearchHits, with the full SearchResponse as meta-data.
  "
  [es indices ^ValueObject vo & {:as options :keys [
    ; extra-source parameters
    query filter from size types sort highlighting only exclude script-fields preference facets named-filters boost explain version min-score
    ; ces/search parameters
    listener ignore-indices routing listener-threaded? search-type operation-threading query-hint scroll source]}]
  {:pre [(instance? ValueObject vo) (or (string? indices) (vector? indices)) (not-empty indices)]}
    (let [
      typefilter {:type {:value (vo-hexname vo)}}
      filter     (map-keywords->hex-fields vo filter)
      filter     (if-not filter (if-not (empty? vo) (vo->term-filter vo))
                  #_else+filter (if-not (empty? vo) {:and (conj [filter] (vo->term-filter vo))}  #_else filter))
      filter     (if filter {:and [typefilter filter]} typefilter)
      fields     (if (or only exclude) (map field-hexname (vo/field-filtered-seq vo only exclude)))
      es-options (into {:format :java, :indices (if (vector? indices) indices [indices])}
        (clojure.core/filter val {
          :listener listener, :ignore-indices ignore-indices, :routing routing, :listener_threaded? listener-threaded?, :search-type search-type
          :operation-threading operation-threading, :query-hint query-hint, :scroll scroll, :source source
          :extra-source (json/encode (into {} (clojure.core/filter val {
            :query            (map-keywords->hex-fields vo query),
            :filter           filter,
            :from             from,
            :size             size,
            :types            types,
            :sort             (map-keywords->hex-fields vo sort),
            :highlighting     (map-keywords->hex-fields vo highlighting),
            :fields           fields,
            :script_fields    (map-keywords->hex-fields vo script-fields),
            :preference       preference,
            :facets           (map-keywords->hex-fields vo facets),
            :named_filters    (map-keywords->hex-fields vo named-filters),
            :boost            boost,
            :explain          explain,
            :version          version,
            :min_score        min-score
          })))
      }))
      response ^SearchResponse (ces/search es es-options)
    ]
      (with-meta
        (map
          (partial SearchHit->ValueObject (if fields SearchHit->fields #_else SearchHit->source) (. vo voCompanion))
          (if-not scroll (.. response hits hits) #_else (scroll-seq es response scroll 0)))
        {:request es-options, :response response})))

(defn ^org.elasticsearch.action.index.IndexRequest ->IndexRequest [^String index ^String type ^String id, input options]
  (let [^"[B" input (if (instance? ValueObject input) (json/encode-smile input) input)
            request (new org.elasticsearch.action.index.IndexRequest index type id)
        {:keys [routing parent timestamp ttl version versionType]} options]
    (. request source input)
    (when routing     (. request routing     routing))
    (when parent      (. request parent      parent))
    (when timestamp   (. request timestamp   timestamp))
    (when ttl         (. request ttl         ttl))
    (when version     (. request version     version))
    (when versionType (. request versionType versionType))
    request))

(defn- patched-update-options [type id options]
  (let [ {:keys [upsert doc]} options
        options (if upsert (assoc options :upsert (->IndexRequest (:index options) type id, upsert options)) #_else options)
        options (if doc    (assoc options :doc    (->IndexRequest (:index options) type id, doc    options)) #_else options) ]
    (conj options {
      :type   type
      :id     id
    })))

(defn update
  [es ^ValueObject vo id & {:as options :keys [index fields]}]
  {:pre [(instance? ValueObject vo) (not (nil? id)) index]}
  (let [type    (Integer/toHexString (.. vo voManifest ID))
        id      (prime.types/to-String id)]
        fields  (map field-hexname fields)
    (ces/update-doc es (patched-update-options type id (assoc options :doc vo)))))

(defn insertAt "Add something to an array with a specific position" [es vo path value pos])

(defn moveTo "Change position of an item in an array" [es vo path pos])

;TODO:
;  Convert fieldnames
(defn appendTo "Add something to the end of an array" [es ^ValueObject vo id & {:as options :keys [index]}]
  {:pre [(instance? ValueObject vo) (not (nil? id)) index]}
  (let [type (Integer/toHexString (.. vo voManifest ID))
        id   (prime.types/to-String id)]
    (ces/update-doc es (patched-update-options type id (assoc options
      :source (json/encode-smile
        (binding [prime.vo/*voseq-key-fn* field-hexname] {
          :script (apply str (map #(str "ctx._source['" % "'] += v" % ";") (keys vo)))
          :params (into  {}  (map #(let [[k v] %1] [(str "v" k) v]) vo))
        }))))
    )))

(defn replaceAt "Replaces a value in an array at given position" [es vo pos value])

(defn removeByValue "Removes an item from an array by value" [es vo val])

#_(defn update-if
  [es ^ValueObject vo id predicate &])

(defn delete [es ^ValueObject vo & {:as options :keys [index]}]
  {:pre [(instance? ValueObject vo)]}
  (assert index ":index required")
  (assert (prime.vo/has-id? vo) (str "vo: " (prn-str vo) " requires an id"))
  (ces/delete-doc es (conj options {
    :id     (prime.types/to-String (.. ^ID vo _id))
    :type   (Integer/toHexString (.. vo voManifest ID))
  })))


;
; Query helpers
;

(defn has-child-vo
  "Construct a 'has_child' query for the given VO type and optional query.
   If no query is given: query is built using vo as term filter, or match_all if vo is empty."
  ([vo] (has-child-vo vo (if (empty? vo) {"match_all" {}} #_else (vo->term-filter vo))))

  ([vo child-query]
    {"has_child" {"type" (vo-hexname vo), "query" child-query}}))

(defn convert [res]
  (ces/convert (:response (meta res)) :clj))
