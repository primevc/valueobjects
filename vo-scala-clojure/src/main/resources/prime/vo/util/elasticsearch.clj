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
  (:import [prime.types EnumValue package$ValueType package$ValueTypes$Tdef package$ValueTypes$Tarray package$ValueTypes$Tenum]
           [prime.vo ValueObject ValueObjectField ValueObjectCompanion ID]
           [com.fasterxml.jackson.core JsonGenerator]

           org.elasticsearch.action.search.SearchResponse, [org.elasticsearch.search SearchHit, SearchHitField],
           org.elasticsearch.action.get.GetResponse))

(set! *warn-on-reflection* true)

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
        package$ValueTypes$Tarray (mapping-field-type-name (. ^package$ValueTypes$Tarray valueType innerType)))))

(declare vo-mapping)

(defn field-mapping
  ([option-map ^ValueObjectField field unique]
    (field-mapping (conj (if unique {:index "not_analyzed"} #_else {}) option-map)
                   (. field valueType) (. field id) (. field keyword)))

  ([option-map, ^package$ValueType value-type, id, field-key]
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
                {:type (mapping-field-type-name (.valueType (._id ^prime.vo.IDField (. empty voManifest))))}
              #_else
                  (vo-mapping empty (or option-map {})))

              option-map
          ))

        (= :prime.types/Interval field-key)
          {:properties {:s {:type "date"} :e {:type "date"}}}

        :else nil)

      option-map ;overwrites defaults
    )}))


(defn vo-mapping "Create an ElasticSearch compatible mapping from an empty ValueObject.
  options are
  - :only    exclusive #{set} of fields to include in mapping
  - :exclude #{set} of fields not to include in mapping
  - any ElasticSearch option.
  "
  ([^ValueObject vo] (vo-mapping vo {}))

  ([^ValueObject vo, option-map]
   (let [id-field (._id ^prime.vo.IDField (. vo voManifest))]
    { :type       "object"
      :dynamic    "strict"
      :properties
      (into {}
        (map
          (fn [^ValueObjectField field] (field-mapping (option-map (.keyword field)) field (identical? id-field field)))
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
    (keyword? expr)
      (or (keyword->hex-field vo expr) expr)
    (vector? expr) ; Clojure wart: A MapEntry is a vector, but not a PersistentCollection :-S and does not implement (empty ..)
      (vec (map (partial map-keywords->hex-fields vo) expr))
    (empty expr)
      (into (empty expr) (map (partial map-keywords->hex-fields vo) expr))
    :else expr))


;
; ElasticSearch index management (mapping API)
;

(defn vo-index-mapping-pair [[^ValueObject vo, options]]
  (assert options)
  [ (Integer/toHexString (.. vo voManifest ID)),
    (conj (dissoc (vo-mapping vo options) :type) options) ])

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

(defn encode-instant [^org.joda.time.ReadableInstant in ^JsonGenerator out]
  (.writeNumber out (.getMillis in)))

(defn encode-uri [^org.apache.commons.httpclient.URI in ^JsonGenerator out]
  (.writeString out (.toString in)))

(doseq [add-encoder [cheshire.generate/add-encoder, cheshire.custom/add-encoder]]
  (add-encoder prime.types.EnumValue              encode-enum)
  (add-encoder prime.vo.ValueObject               encode-vo)
  (add-encoder org.apache.commons.httpclient.URI  encode-uri)
  (add-encoder org.joda.time.ReadableInstant      encode-instant))

(alter-var-root #'cheshire.generate/generate
  (fn [orig-generate]
    (fn [^JsonGenerator jg obj ^String date-format ^Exception ex]
      (binding [prime.vo/*voseq-key-fn* identity]
        (if (instance? ValueObject obj) (encode-vo jg obj date-format ex)
        #_else                          (orig-generate jg obj date-format ex))))))


;
; ElasticSearch querying API
;

(def-valuesource ElasticSearch-ValueSource [^int type, ^java.util.Map jmap, response]
  (typeID   [this, base] (if (not (== -1 type)) type #_else base))

  (contains [this, name idx]          (.containsKey jmap (Integer/toHexString (bit-shift-right idx 8))))
  (anyAt    [this, name idx notFound] (or
    (if jmap
      (let [item (.get jmap (Integer/toHexString (bit-shift-right idx 8)))]
        (condp instance? item
          SearchHitField
            (.value ^SearchHitField item)

          java.util.Map
            (let [item ^java.util.Map item]
              (if-let [x (.get item "x")] x
              (if-let [v (.get item "v")] v
              (ElasticSearch-ValueSource. (or (.get item "t") -1), item, response))))

          item)))
    notFound)))

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
  (assert vo    "vo required")
  (ces/index-doc es (conj options {
    :id     (.. ^ID vo _id toString)
    :type   (Integer/toHexString (.. vo voManifest ID))
    :source (json/encode-smile vo)
  })))

(defn SearchHit->fields [^SearchHit sh] (.fields sh)     )
(defn SearchHit->source [^SearchHit sh] (.sourceAsMap sh))

(defn SearchHit->ValueObject [source-fn, ^ValueObjectCompanion voCompanion, ^SearchHit hit]
  (.apply voCompanion
    (ElasticSearch-ValueSource. (Integer/parseInt (.type hit) 16) (source-fn hit) hit)))

(defn search
  "options: see clj-elasticsearch.client/search
  example: (vo/search {:filter {:term {:name 'Henk'}}})

  Set indices to [\"*\"] to search in all.

  Returns: seq of SearchHits, with the full SearchResponse as meta-data.
  "
  [es ^ValueObject vo indices & {:as options :keys [
    ; extra-source parameters
    query filter from size types sort highlighting only exclude script-fields preference facets named-filters boost explain version min-score
    ; ces/search parameters
    listener ignore-indices routing listener-threaded? search-type operation-threading query-hint scroll source]}]
  {:pre [(instance? ValueObject vo) (or (string? indices) (vector? indices)) (not-empty indices)]}
    (let [
      filter     (map-keywords->hex-fields vo filter)
      filter     (if-not filter (if-not (empty? vo) (vo->term-filter vo))
                  #_else+filter (if-not (empty? vo) {:and (conj [filter] (vo->term-filter vo ""))}  #_else filter))
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
          (.. response hits hits))
        {:request es-options, :response response})))

(defn update
  [es ^ValueObject vo id & {:as options :keys [index]}]
  {:pre [(instance? ValueObject vo) (not (nil? id))]}
  (ces/update-doc es (conj options {
    :type (Integer/toHexString (.. vo voManifest ID))
    :id (str id)
    :doc (binding [prime.vo/*voseq-key-fn* field-hexname] (json/encode-smile vo))
    })))

(defn insertAt "Add something to an array with a specific position" [es vo path value pos])

(defn moveTo "Change position of an item in an array" [es vo path pos])

;TODO 
;  Convert fieldnames
;  Put values to params
(defn appendTo "Add something to the end of an array" [es vo id & {:as options :keys [index]}]
  {:pre [(instance? ValueObject vo) (not (nil? id))]}
  (ces/update-doc es (conj options {
    :type (Integer/toHexString (.. vo voManifest ID))
    :id (str id)
    :script (apply str (map #(str "ctx._source." (name (key %)) " += " (val %) ";") vo))
    })))

(defn replaceAt "Replaces a value in an array at given position" [es vo pos value])

(defn removeByValue "Removes an item from an array by value" [es vo val])

#_(defn update-if
  [es ^ValueObject vo id predicate &])

(defn delete [es ^ValueObject vo & {:as options :keys [index]}]
  {:pre [(instance? ValueObject vo)]}
  (assert index ":index required")
  (assert vo    "vo required")
  (ces/delete-doc es (conj options {
    :id     (.. ^ID vo _id toString)
    :type   (Integer/toHexString (.. vo voManifest ID))
  })))
