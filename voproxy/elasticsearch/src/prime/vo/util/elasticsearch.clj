;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.util.elasticsearch
  "The implementation of the ElasticSearch VOProxy and VOSearchProxy."
  (:refer-clojure :exclude (get))
  (:require [prime.vo :as vo]
            [clojure.set     :as set]
            [cheshire [core :as core] [generate :as gen]]
            [clj-elasticsearch.client :as ces]
            [prime.vo.source :refer (def-valuesource)]
            [prime.vo.pathops :as pathops]
            [prime.vo.util.json :as json] ; For loading the right VO encoders in Cheshire.
            [prime.vo.util.elasticsearch.script :as escript])
  (:import [prime.types EnumValue package$ValueType package$ValueTypes$Tdef
            package$ValueTypes$Tarray package$ValueTypes$Tenum]
           [prime.vo IDField ValueObject ValueObjectManifest ValueObjectField ValueObjectCompanion ID]
           [com.fasterxml.jackson.core JsonGenerator]
           [org.elasticsearch.action.get GetResponse]
           [org.elasticsearch.action.search SearchResponse]
           [org.elasticsearch.action.index IndexResponse]
           [org.elasticsearch.search SearchHit SearchHitField]))


;;; Demonize TransportClient, if Immutant is available.

(try
  (require 'immutant.daemons)
  (eval '(extend-type org.elasticsearch.client.Client
    immutant.daemons/Daemon
    (start [this])
    (stop  [this] (.close this))))
  (catch Exception e))

(defn create-client [hosts cluster-name & options]
  (ces/make-client :transport {:hosts hosts :cluster-name cluster-name}))


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
      [(str k ".x") (. v toString)]
    #_else
      [(str k ".v") (. v value   )]))

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
  [ (Integer/toHexString (.. vo voManifest ID)),
    (conj (dissoc (vo-mapping vo options) :type) options) ])

(defn put-mapping [es index-name, vo-options-pair]
  (if (map? vo-options-pair)
    (doseq [pair vo-options-pair]
      (put-mapping es index-name pair))
  ;else
  (let [[type mapping] (vo-index-mapping-pair vo-options-pair)]
    (ces/put-mapping es, {
      :ignore-conflicts? true
      :index  index-name
      :type   type
      :source {type mapping}
    }))))

(defn index-exists? [es indices]
  {:pre [(or (string? indices) (vector? indices)) (not-empty indices)]}
  (-> (ces/exists-index es
        {:indices (if (instance? String indices) [indices] #_else indices)})
      :exists))

(defn create-index
  ([es index-name vo->options-map]
    (create-index es index-name vo->options-map {}))
  ([es index-name vo->options-map root-options]
    (try
      (ces/create-index es, {
        :index  index-name
        :source (assoc root-options :mappings (into {} (map vo-index-mapping-pair vo->options-map)))
      })
    (catch org.elasticsearch.indices.IndexAlreadyExistsException e
      (put-mapping es index-name vo->options-map)))))

(defn- finish-change
  [proxy options retval]
  (when (:refresh-at-change (merge (:default-opts proxy) options))
    (ces/refresh-index (:client proxy) {:indices ["*"]}))
  retval)


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
    (let [terms (vo->term-filter vo "")]
      (if-not (empty? terms)
        {:and (mapv #(hash-map :term %) terms)})))

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
                                                                  [{k %}])
                                                               v)))

                 :else
                 (recur (rest items) (conj terms (apply hash-map (term-kv-pair v k pair)))))
           terms)))))

(defn vo->search-filter [vo]
  (let [term-filter (vo->term-filter (vo/without-id vo))]
    (if (prime.vo/has-id? vo) (conj term-filter {"ids" {"values" [(:id vo)]}})
    #_else term-filter)))


(defn vo-id->str [vo]
  (-> vo :id prime.types/to-String))


(defn get
  "options: see clj-elasticsearch.client/get-doc"
  [{:keys [client index default-opts] :as proxy} ^ValueObject vo options]
  {:pre [client (string? index) (vo/has-id? vo) options]}
  (let [^GetResponse resp (ces/get-doc client
                                       (assoc (merge default-opts options)
                                         :type (Integer/toHexString (.. vo voManifest ID))
                                         :index index
                                         :format :java
                                         :id (vo-id->str vo)))]
    (when (.isExists resp)
      (.apply (. vo voCompanion)
              (ElasticSearch-root-ValueSource. (.. vo voManifest ID) (.. vo voManifest) (.getSourceAsMap resp) (:id vo))))))

(defn put
  "options: see clj-elasticsearch.client/index-doc"
  [{:keys [client index default-opts] :as proxy} ^ValueObject vo options]
  {:pre [client index]}
  (assert (vo/has-id? vo) (print-str "VO requires an id: " vo))
  (let [options (merge default-opts options)
        resp (ces/index-doc client
                            (assoc options
                              :index index
                              :id (vo-id->str vo)
                              :type (Integer/toHexString (.. vo voManifest ID))
                              :source (generate-hexed-fields-smile (vo/without-id vo))))]
    (finish-change proxy options resp)))


(defn- patched-update-options
  [type id options]
  (let [{:keys [doc] :as options} (assoc options :type type :id id)]
    (if doc
      (assoc options :doc (generate-hexed-fields-smile doc))
      options)))


(defn update
  [{:keys [client index default-opts] :as proxy} ^ValueObject vo id {:as options :keys [fields]}]
  {:pre [(instance? ValueObject vo) (not (nil? id)) index client]}
  (let [type (Integer/toHexString (.. vo voManifest ID))
        id (prime.types/to-String id)
        fields (map field-hexname fields)]
    (->> (merge default-opts {:index index :doc (vo/without-id vo) :doc-as-upsert? true} options)
         (patched-update-options type id)
         (ces/update-doc client)
         (finish-change proxy options))))


(defn delete
  [{:keys [client index default-opts] :as proxy} ^ValueObject vo options]
  {:pre [(instance? ValueObject vo) index client (vo/has-id? vo)]}
  (->> (ces/delete-doc client (merge default-opts
                                     {:index index
                                      :id (vo-id->str vo)
                                      :type (Integer/toHexString (.. vo voManifest ID))}
                                     options))
       (finish-change proxy options)))


;;; Search.

(defn SearchHit->fields [^SearchHit sh] (.fields sh)     )
(defn SearchHit->source [^SearchHit sh] (.sourceAsMap sh))

(defn SearchHit->ValueObject [source-fn, ^ValueObject empty-vo, ^SearchHit hit]
  (.apply (. empty-vo voCompanion)
    (ElasticSearch-root-ValueSource. (Integer/parseInt (.type hit) 16) (.. empty-vo voManifest) (source-fn hit) (.id hit))))

(defn scroll-seq [es, ^SearchResponse scroll-req keep-alive, from]
  (let [ ^SearchResponse response (ces/scroll es {:scroll-id (.getScrollId scroll-req) :scroll keep-alive, :format :java})
          hits     (.. response getHits hits)
          num-hits (.. response getHits totalHits)
          last-hit (+  from (count hits)) ]
    (if (and (.getScrollId response) (< last-hit num-hits))
      (concat hits (lazy-seq (scroll-seq es response keep-alive last-hit)))
    #_else    hits)))


(def search-cljes-opts
  [:listener :ignore-indices :routing :listener-threaded? :search-type :operation-threading
   :query-hint :scroll :source])

(def search-extra-source-opts
  [:query :filter :from :size :types :sort :highlighting :script-fields :preference :facets
   :named-filters :boost :explain :version :min-score])

(def need-hex-map-opts
  [:query :sort :highlighting :script-fields :facets :named-filters])


;; ---TODO add "type" filter by default.
;; ---TODO use id->voCompanion when type filter is removed, to allow searching for multiple types.
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
        typefilter {:type {:value (vo-hexname vo)}}
        filter (map-keywords->hex-fields vo filter)
        filter (if-not filter
                 (when-not (empty? vo)
                   (vo->search-filter vo))
                 (if-not (empty? vo)
                   {:and (conj [filter] (vo->search-filter vo))}
                   filter))
        filter (if filter {:and [typefilter filter]} typefilter)
        fields (when (or only exclude)
                 (map field-hexname (vo/field-filtered-seq vo only exclude)))
        extra-source-opts (->> (map (fn [[k v]] [k (map-keywords->hex-fields vo v)])
                                    (select-keys options need-hex-map-opts))
                               (into {:fields fields :filter filter})
                               (merge (select-keys options search-extra-source-opts))
                               (clojure.core/filter val)
                               (into {}))
        es-options (assoc (into {} (clojure.core/filter val (select-keys options search-cljes-opts)))
                     :format (clojure.core/get options :format :java)
                     :indices (clojure.core/get options :indices [index])
                     :extra-source (generate-hexed-fields-smile extra-source-opts))
        ;; _ (clojure.pprint/pprint (assoc es-options :extra-source extra-source-opts))
        ^SearchResponse
        response (ces/search client es-options)]
      (with-meta
        (map (partial SearchHit->ValueObject
                      (if fields SearchHit->fields SearchHit->source)
                      vo)
             (if-not scroll
               (.. response getHits hits)
               (scroll-seq client response scroll 0)))
        {:request es-options, :response response})))


;;; Delta change functions.

(defn- update-by-script
  [{:keys [client index default-opts] :as proxy} ^ValueObject vo path path-vars options script params]
  {:pre [(instance? ValueObject vo) (not (nil? (:id vo))) index client (string? script)]}
  (let [path (hexify-path vo (pathops/fill-path path path-vars))
        type (Integer/toHexString (.. vo voManifest ID))]
    (->> (ces/update-doc client {:index index
                                 :type type
                                 :id (vo-id->str vo)
                                 :source (generate-hexed-fields-smile
                                          {:script script
                                           :lang "clj"
                                           :params (assoc params :path path)})})
         (finish-change proxy options))))


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

(defn convert [res]
  (ces/convert (:response (meta res)) :clj))
