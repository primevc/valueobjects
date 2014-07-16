;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.util.elasticsearch
  "The implementation of the ElasticSearch VOProxy and VOSearchProxy."
  (:refer-clojure :exclude (get))
  (:require [prime.vo :as vo]
            [clj-tuple :refer (tuple)]
            [clojure.set     :as set]
            [cheshire [core :as core] [generate :as gen]]
            [clojurewerkz.elastisch.native :as es]
            [clojurewerkz.elastisch.native.conversion :as cnv]
            [clojurewerkz.elastisch.native.document :as doc]
            [clojurewerkz.elastisch.native.index :as idx]
            [prime.vo.source :refer (def-valuesource)]
            [prime.vo.pathops :as pathops]
            [prime.vo.util.json :as json] ; For loading the right VO encoders in Cheshire.
            [prime.vo.util.elasticsearch.script :as escript])
  (:import [prime.types EnumValue package$ValueType package$ValueTypes$Tdef
            package$ValueTypes$Tarray package$ValueTypes$Tenum]
           [prime.vo IDField ValueObject ValueObjectManifest ValueObjectField ValueObjectCompanion ID]
           [com.fasterxml.jackson.core JsonGenerator]
           [org.elasticsearch.action.get GetResponse]
           [org.elasticsearch.action.search SearchRequest SearchResponse]
            org.elasticsearch.common.xcontent.XContentType
           [org.elasticsearch.action.index IndexRequest IndexResponse]
           [org.elasticsearch.action.update UpdateRequest UpdateResponse]
           [org.elasticsearch.search SearchHit SearchHitField]))


;;; Demonize TransportClient, if Immutant is available.

(try
  (require 'immutant.daemons)
  (eval '(extend-type org.elasticsearch.client.Client
    immutant.daemons/Daemon
    (start [this])
    (stop  [this] (.close this))))
  (catch Exception e))


;;; Generate ElasticSearch mapping from VO.

(defprotocol TermFilter "Used while creating mappings and term-filter queries from a ValueObject."
  (term-kv-pair [value key kv-pair] "Returns a vector of [^String key, value] which gets appended to the term-filter map.")
  (mapping-field-type-defaults [valueType] "Returns a map with default options for ElasticSearch mapping."))

(extend-protocol TermFilter

  Object
  (term-kv-pair [v k kv-pair] kv-pair)

  EnumValue
  (term-kv-pair [v k kv-pair]
    (if (.isInstance scala.Product v)
      (tuple (str k ".x") (. v toString))
    #_else
      (tuple (str k ".v") (. v value))))

  (mapping-field-type-defaults [value]
    {:properties {
      :v {:type "integer"}
      :x {:type "string", :index "not_analyzed"} ;x = extended value, currently only String
    }})

  package$ValueType
  (mapping-field-type-defaults [valueType]
    (case (. valueType keyword)
      :prime.types/boolean    nil
      :prime.types/integer    nil
      :prime.types/decimal    nil
      :prime.types/Date       nil
      :prime.types/Date+time  nil
      :prime.types/Interval   {:properties {:s {:type "date"} :e {:type "date"}}}
      :prime.types/Color      nil
      :prime.types/String     nil
      :prime.types/URI        {:index "not_analyzed"}
      :prime.types/URL        {:index "not_analyzed"}
      :prime.types/E-mail     {:index "not_analyzed"}
      :prime.types/ObjectId   {:index "not_analyzed"}
      :prime.types/FileRef    {:index "not_analyzed"}
      #_default
        (condp instance? valueType
          package$ValueTypes$Tdef   (mapping-field-type-defaults (.. ^package$ValueTypes$Tdef  valueType empty))
          package$ValueTypes$Tenum  (mapping-field-type-defaults (.. ^package$ValueTypes$Tenum valueType t valueSet first))
          package$ValueTypes$Tarray (assert false "array should be mapped as it's contents"))))

  prime.vo.ValueObject
  (mapping-field-type-defaults [vo] nil)
)

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
           ;:index_name (name field-key)
           :type       (mapping-field-type-name value-type)}
      (if (and
            (nil? (mapping-field-type-defaults value-type))
            (instance? package$ValueTypes$Tdef  value-type))
          (let [^package$ValueTypes$Tdef value-type value-type
                ^ValueObject             empty      (.. value-type empty)]
            (conj
              (if (.. value-type ref)
                {:type (mapping-field-type-name (.valueType (._id ^IDField (. empty voManifest)))) :index "not_analyzed"}
              #_else
                (vo-mapping empty (or option-map {})))

              (dissoc option-map :type)
          ))
        #_else
          (conj {} (mapping-field-type-defaults value-type) option-map) ;overwrites defaults
    ))
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
   (let [id-field  (vo/id-field vo)
         field-set (vo/field-filtered-seq vo (:only option-map) (:exclude option-map))]
    (let [unknown-options
          (set/difference
            (set (keys option-map))
            (set (map vo/keyword field-set))
            #{ :exclude :only } )]
      (assert (empty? unknown-options) (print-str "\n\tMapping non-existant field(s):" unknown-options "\n\t " vo)))
    { :type       "object"
      :dynamic    "strict"
      :properties
      (into {}
        (cons
          {"t" {:type "integer", :store "no"}},
          (map
            (fn [^ValueObjectField field] (field-mapping (option-map (.keyword field)) field (if id-field (identical? id-field field))))
            field-set)
          ))
    }))
)

(defn- ^String field-hexname [^ValueObjectField field]
  (Integer/toHexString (.id field)))

(defn- encode-enum [^EnumValue in ^JsonGenerator out]
  (.writeStartObject out)
  (if-not (.isInstance scala.Product in)
    (.writeNumberField out "v", (.value in))
    (.writeObjectField out "x", (.toString in)))
  (.writeEndObject out))

(defn hexify-path "Recursively walks path.
  Maps keywords to hex-names (for JSON access) and keeps all other values as they are."
  [^ValueObject vo path]
  (vo/fields-path-seq vo path field-hexname))

(defn keyword->hex-field   [^prime.vo.ValueObject vo, ^clojure.lang.Named key]
  (if-let [path (vo/fields-path-seq vo (clojure.string/split (name key) #"[.]") field-hexname)]
    (clojure.string/join "." (filter string? path))))

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
    (= expr <=)  "lte"  (= expr <)   "lt"
    (= expr >=)  "gte"  (= expr >)   "gt"
    (= expr not) "not"
    :else expr))

(defn vo-hexname [^ValueObject vo] (Integer/toHexString (.. vo voManifest ID)))

(defmacro map-enum-as-integer
  [enum-class]
  `(do
     (gen/add-encoder ~enum-class
                  (fn [^prime.types.EnumValue in# ^JsonGenerator out#]
                    (.writeNumber out# (. in# value))))
     (extend-type ~enum-class, TermFilter
                  (term-kv-pair [v# k# kv-pair#] [k# (. v# value)])
                  (mapping-field-type-defaults [value#] {:type "integer"}) )))

(defmacro map-enum-as-string
  [enum-class string-method]
  `(do
     (gen/add-encoder# ~enum-class
                   (fn [^prime.types.EnumValue in# ^JsonGenerator out#]
                     (.writeString out# (. in# ~string-method))))
     (extend-type ~enum-class, TermFilter
                  (term-kv-pair [v# k# kv-pair#] [k# (. v# ~string-method)])
                  (mapping-field-type-defaults [value#] {:type "string"}) )))

(defmacro map-as-string-type
  [class elasticsearch-type to-string-fn]
  `(do
     (gen/add-encoder ~class
                      (fn [in# ^JsonGenerator out#]
                        (.writeString out# (~to-string-fn in#))))
     (extend-type ~class, TermFilter
                  (term-kv-pair [v# k# kv-pair#] [k# (~to-string-fn v#)])
                  (mapping-field-type-defaults [value#] {:type ~elasticsearch-type}) )))


;;; ElasticSearch index management (mapping API).

(defn vo-index-mapping-pair [[^ValueObject vo, options]]
  (assert options)
  [ (vo-hexname vo),
    (dissoc (vo-mapping vo options) :type :exclude :only) ])

(defn put-mapping [es index-name, vo-options-pair]
  (if (map? vo-options-pair)
    (doseq [pair vo-options-pair]
      (put-mapping es index-name pair))
  ;else
    (let [[type mapping] (vo-index-mapping-pair vo-options-pair)]
      (idx/update-mapping es, index-name, type, :ignore-conflicts true, :mapping mapping))))

(defn create-index
  ([client index-name vo->options-map]
    (create-index client index-name vo->options-map {}))
  ([client index-name vo->options-map root-options]
    (try
      (apply idx/create client index-name
             (->> (into {} (map vo-index-mapping-pair vo->options-map))
                  (assoc root-options :mappings)
                  (mapcat identity)))
    (catch org.elasticsearch.indices.IndexAlreadyExistsException e
      (put-mapping client index-name vo->options-map)))))


;;; ElasticSearch ValueSource.

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
      (if-let [x (.get item "x")]
        x
        (if-let [v (.get item "v")]
          v
          (ElasticSearch-ValueSource. (or (.get item "t") -1), item, nil))))

    java.util.List
    (map convert-search-result item)

    item))

(defn- is-id-value? [manifest, id-value, name]
  (and (not (nil? id-value))
       (= name (.. ^IDField manifest _id name))))

(def-valuesource ElasticSearch-root-ValueSource [^int type, ^ValueObjectManifest manifest, ^java.util.Map jmap, search-hit-id]
  (typeID   [this, base] (if (not (== -1 type)) type #_else base))

  (contains [this, name idx]
    (or (is-id-value? manifest search-hit-id, name)
        (.containsKey jmap (Integer/toHexString (bit-shift-right idx 8)))))

  (anyAt    [this, name idx notFound]
    (cond
      (is-id-value? manifest search-hit-id, name)
      search-hit-id

      jmap
      (let [item (.get jmap (Integer/toHexString (bit-shift-right idx 8)))]
        (convert-search-result item))

      :else notFound)))


;;; ElasticSearch querying API.

(defn- generate-hexed-fields-smile [obj]
  (binding [json/*field-transform-fn* field-hexname
            json/*encode-enum-fn* encode-enum]
    (core/generate-smile obj)))


(defn vo->term-filter
  ([vo]
    (let [terms (vo->term-filter vo "")
          terms (mapv #(hash-map :term %) terms)]
      (if (second terms) {:and terms}
       #_else (first terms))))

  ([vo prefix]
     (binding [vo/*voseq-key-fn* #(str prefix (field-hexname %))]
       (loop [items (seq vo)
              terms []]
         (if-let [[k v :as pair] (first items)]
           (cond (map? v)
                 (recur (rest items) (concat terms (vo->term-filter v (str k "."))))

                 (vector? v)
                 (recur (rest items) (apply concat terms (mapv #(if (instance? ValueObject %)
                                                                  (vo->term-filter % (str k "."))
                                                                  (tuple {k %}))
                                                               v)))

                 :else
                 (recur (rest items) (conj terms (apply hash-map (term-kv-pair v k pair)))))
           terms)))))

(defn vo->search-filter [vo]
  (let [term-filter (vo->term-filter (vo/without-id vo))
        ids-filter  (when (prime.vo/has-id? vo) {"ids" {"values" [(:id vo)]}})]
    (if ids-filter
      (if-not term-filter ids-filter #_else {:and [term-filter ids-filter]})
    #_else term-filter)))


(defn vo-id->str [vo]
  (-> vo :id prime.types/to-String))


(defn get
  "Options are  :parent, :preference, :routing, and :fields"
  [{:keys [client index default-opts] :as proxy} ^ValueObject vo options]
  {:pre [client (string? index) (vo/has-id? vo) options]}
  (let [ft (es/get client (cnv/->get-request index
                                             (vo-hexname vo)
                                             (vo-id->str vo)
                                             (merge default-opts options)))
        ^GetResponse resp (.actionGet ft)]
    (when (.isExists resp)
      (.apply (. vo voCompanion)
              (ElasticSearch-root-ValueSource. (.. vo voManifest ID) (.. vo voManifest) (.getSourceAsMap resp) (:id vo))))))



(defn ^IndexRequest ->index-request
  "Builds an index action request.
   Copied from clojurewerkz.elastisch.native.conversion and modified to always generate \"smile\"."
  ([index mapping-type doc]
     ;; default content type used by IndexRequest is JSON. MK.
     (-> (IndexRequest. (name index) (name mapping-type))
         (.source ^bytes (generate-hexed-fields-smile doc))))
  ;; non-variadic because it is more convenient and efficient to
  ;; invoke this internal implementation fn this way. MK.
  ([index mapping-type doc {:keys [id
                                   routing
                                   parent
;                                   timeout
                                   replication
                                   consistency
                                   timestamp
                                   ttl
                                   op-type
                                   refresh, refresh-at-change
;                                   fields
                                   version
                                   version-type]}]
     (let [ir (-> (IndexRequest. (name index) (name mapping-type))
                  (.source ^bytes (generate-hexed-fields-smile doc)))]
       (.contentType ir XContentType/SMILE)
       (when id
         (.id ir ^String id))
       (when routing
         (.routing ir ^String routing))
       (when parent
         (.parent ir ^String parent))
       (when timestamp
         (.timestamp ir timestamp))
       (when ttl
         (.ttl ir ttl))
       (when consistency
         (.consistencyLevel ir (org.elasticsearch.action.WriteConsistencyLevel/fromString (name consistency))))
       (when replication
         (.replicationType ir (org.elasticsearch.action.support.replication.ReplicationType/fromString (name replication))))
       (when op-type
         (.opType ir ^String (.toLowerCase (name op-type))))
       (when (or refresh refresh-at-change)
         (.refresh ir true))
       (when version
         (.version ir version))
       (when version-type
         (.versionType ir (cnv/to-version-type version-type)))
       ir)))


(defn put
  "Options are  :routing, :parent, :timestamp, :ttl, :refresh, :version and :version-type"
  [{:keys [client index default-opts] :as proxy} ^ValueObject vo options]
  {:pre [client index]}
  (assert (vo/has-id? vo) (print-str "VO requires an id: " vo))
  (let [options (merge default-opts
                       {:id (vo-id->str vo)
                        :op-type "index"}
                       options)
        req (es/index client (->index-request index
                                              (vo-hexname vo)
                                              (vo/without-id vo)
                                              (merge default-opts options)))
        resp (cnv/index-response->map (.actionGet req))]))



(defn ^UpdateRequest ->update-request
  ([index-name mapping-type ^String id doc {:keys [upsert
                                                   doc-as-upsert?
                                                   routing
                                                   parent
                                                   timeout
                                                   replication
                                                   consistency
                                                   ttl
                                                   refresh, refresh-at-change
                                                   fields
                                                   version
                                                   version-type
                                            ] :or {doc-as-upsert? true} :as options}]
    (when doc-as-upsert?
      (assert (= nil upsert) "Ambiguous: Should I use `doc` as upsert? or the given `upsert`?"))
    (let [r (UpdateRequest. (name index-name) (name mapping-type) id)]
       (when doc
        (.doc r ^bytes (generate-hexed-fields-smile doc)))
       (when upsert
        (.upsert r ^bytes (generate-hexed-fields-smile upsert)))
       (when doc-as-upsert?
        (.docAsUpsert r true))
       (when routing
         (.routing r ^String routing))
       (when parent
         (.parent r ^String parent))
       (when timeout
         (.timeout r ^String timeout))
       (when consistency
         (.consistencyLevel r (org.elasticsearch.action.WriteConsistencyLevel/fromString (name consistency))))
       (when replication
         (.replicationType r (org.elasticsearch.action.support.replication.ReplicationType/fromString (name replication))))
       (when (or refresh refresh-at-change)
         (.refresh r true))
       (when fields
         (.fields r fields))
       (when version
         (.version r version))
       (when version-type
         (.versionType r (cnv/to-version-type version-type)))
       r)))

(defn update
  "Updates or creates a document using provided data.
   Options are  :doc-as-upsert? – default is true
           and  :upsert – the document to insert if doc with id doesn't exists"
  [{:keys [client index default-opts] :as proxy} ^ValueObject vo id {:keys [fields] :as options}]
  {:pre [(instance? ValueObject vo) (not (nil? id)) index client]}
  (->> (->update-request (name index)
                         (vo-hexname vo)
                         (prime.types/to-String id)
                         (vo/without-id vo)
                         (merge default-opts
                                (if fields (assoc options :fields (map field-hexname fields))
                                 #_else options)))
       (es/update client)
       (.actionGet)
       (cnv/update-response->map)))



(defn delete
  [{:keys [client index default-opts] :as proxy} ^ValueObject vo options]
  {:pre [(instance? ValueObject vo) index client (vo/has-id? vo)]}
  (apply doc/delete client index (vo-hexname vo) (vo-id->str vo)
         (->> (let [options (merge default-opts options)]
                (if (:refresh-at-change options) (assoc options :refresh true)
                 #_else options))
             (mapcat identity))))


;;; Search.

(defn SearchHit->ValueObject [^ValueObject empty-vo, ^SearchHit hit]
  (.apply (. empty-vo voCompanion)
    (ElasticSearch-root-ValueSource. (Integer/parseInt (.type hit) 16) (.. empty-vo voManifest) (.sourceAsMap hit) (.id hit))))

(defn scroll-seq [es, ^SearchResponse scroll-req keep-alive, from]
  (let [ft (es/search-scroll es (cnv/->search-scroll-request (.getScrollId scroll-req)
                                                             {:scroll keep-alive}))
         ^SearchResponse response (.actionGet ft)
          hits     (.. response getHits hits)
          num-hits (.. response getHits totalHits)
          last-hit (+  from (count hits)) ]
    (if (and (.getScrollId response) (< last-hit num-hits))
      (concat hits (lazy-seq (scroll-seq es response keep-alive last-hit)))
    #_else    hits)))


(def search-source-opts
  ;; Based on all fields generated by `SearchSourceBuilder.toXContent()`
  ;; https://github.com/elasticsearch/elasticsearch/blob/master/src/main/java/org/elasticsearch/search/builder/SearchSourceBuilder.java
  [:from :size :timeout :query :post_filter :filter :min_score :version :explain :_source
   :fields :fielddata_fields :script_fields :sort :track_scores :indices_boost :facets
   :aggregations :highlight :suggest :rescore :stats])

(def need-hex-map-opts
  [:query :sort :highlighting :script_fields :facets :named_filters])

(defn ^SearchRequest ->search-request
  [index-name mapping-type {:keys [search-type search_type routing preference
                                   template-source template-name template-params
                                   source extra-source scroll ;indices-options
                                   ]}]
  (let [r (SearchRequest.)]
    ;; non-source
    (when index-name
      (.indices r (cnv/->string-array index-name)))
    (when mapping-type
      (.types r (cnv/->string-array mapping-type)))
    (when-let [s (or search-type search_type)]
      (.searchType r ^String (name s)))
    (when routing
      (.routing r ^String routing))
    (when preference
      (.preference r ^String preference))
    (when template-source
      (.templateSource r ^bytes template-source))
    (when template-name
      (.templateName r ^bytes template-name))
    (when template-params
      (.templateParams r ^bytes template-params))
    (when source
      (.source r ^bytes source))
    (when extra-source
      (.extraSource r ^bytes extra-source))
    (when scroll
      (.scroll r ^String scroll))
    r))

;; ---TODO add ability to search multiple VO types (and remove type filter)
;; ---TODO use id->voCompanion when type filter is removed
(defn search
  "options: see clj-elasticsearch.client/search
  example: (vo/search {:filter {:term {:name 'Henk'}}})

  Set indices to \"*\" to search in all.

  Returns: seq of SearchHits, with the full SearchResponse as meta-data.
  "
  [{:keys [client index default-opts] :as proxy}
   ^ValueObject vo
   {:keys [filter only exclude size scroll] :as options}]
  {:pre [(instance? ValueObject vo) (string? index) (not (and size scroll)) client]}
  (let [options (merge default-opts options)
        filter (map-keywords->hex-fields vo filter)
        filter (if-not filter
                 (when-not (empty? vo)
                   (vo->search-filter vo))
                 (if-not (empty? vo)
                   {:and (conj [filter] (vo->search-filter vo))}
                   filter))
        fields (when (or only exclude)
                 (map #(str (field-hexname %) "*") (vo/field-filtered-seq vo only exclude)))
        extra-source-opts (->> (map (fn [[k v]] (tuple k (map-keywords->hex-fields vo v)))
                                    (select-keys options need-hex-map-opts))
                               (into {:_source fields :filter filter})
                               (merge (select-keys options search-source-opts))
                               (clojure.core/filter val)
                               (into {}))
        es-options (assoc options :source (generate-hexed-fields-smile extra-source-opts))
        ;; _ (clojure.pprint/pprint (assoc es-options :extra-source extra-source-opts))
        ft (es/search client (->search-request index (vo-hexname vo) es-options))
        ^SearchResponse response (.actionGet ft)]
      (with-meta
        (map #(SearchHit->ValueObject vo %)
             (if-not scroll
               (.. response getHits hits)
               (scroll-seq client response scroll 0)))
        {:request es-options, :response response})))


;;; Delta change functions.

(defn- update-by-script
  [{:keys [client index default-opts] :as proxy} ^ValueObject vo path path-vars options script params]
  {:pre [(instance? ValueObject vo) (not (nil? (:id vo))) index client (string? script)]}
  (let [path (hexify-path vo (pathops/fill-path path path-vars))
        req (doto (cnv/->update-request index
                                        (vo-hexname vo)
                                        (vo-id->str vo)
                                        script
                                        nil) ; params are set using smile source below
              (.refresh (or (:refresh-at-change options (:refresh-at-change default-opts))
                            (:refresh options (:refresh default-opts))
                            false))
              (.source ^bytes (generate-hexed-fields-smile {"lang"   "clj"
                                                            "params" (assoc params :path path)})))
        res (es/update client req)]
      (cnv/update-response->map (.actionGet res))))

(defn append-to
  [proxy vo path path-vars value options]
  (update-by-script proxy vo path path-vars options escript/append-to-script {"value" value}))

(defn insert-at
  [proxy vo path path-vars value options]
  (update-by-script proxy vo path path-vars options escript/insert-at-script {"value" value}))

(defn move-to
  [proxy vo path path-vars to options]
  (update-by-script proxy vo path path-vars options escript/move-to-script {"to" to}))

(defn replace-at
  [proxy vo path path-vars value options]
  (update-by-script proxy vo path path-vars options escript/replace-at-script {"value" value}))

(defn merge-at
  [proxy vo path path-vars value options]
  (update-by-script proxy vo path path-vars options escript/merge-at-script {"value" value}))

(defn remove-from
  [proxy vo path path-vars options]
  (update-by-script proxy vo path path-vars options escript/remove-from-script nil))


;;; Query helpers

(defn has-child-vo
  "Construct a 'has_child' query for the given VO type and optional query.
   If no query is given: query is built using vo as term filter, or match_all if vo is empty."
  ([vo] (has-child-vo vo (if (empty? vo) {"match_all" {}} #_else (vo->search-filter vo))))

  ([vo child-query]
    {"has_child" {"type" (vo-hexname vo), "query" child-query}}))
