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
  (:import [prime.types package$ValueType package$ValueTypes$Tdef package$ValueTypes$Tarray package$ValueTypes$Tenum]
           [com.fasterxml.jackson.core JsonGenerator]))

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
  ([option-map ^ValueObjectField field]
    (field-mapping (or option-map {}) (. field valueType) (. field id) (. field keyword)))

  ([option-map ^package$ValueType value-type, id, field-key]
    { (Integer/toHexString id),
    (conj {:store "no"
           :index_name (name field-key)
           :type       (mapping-field-type-name value-type)}
      (cond
        (instance? package$ValueTypes$Tenum value-type)
          {:properties {:v {:type "integer"} :x {:type "string"}}} ;x = extended value, currently only String

        (instance? package$ValueTypes$Tdef  value-type)
          (let [^package$ValueTypes$Tdef value-type value-type
                ^ValueObject             empty      (.. value-type empty)]
            (conj
              (if (.. value-type ref)
                {:type (mapping-field-type-name (.. empty voManifest (_id) valueType))}
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
    { :type       "object"
      :dynamic    "strict"
      :properties
      (into {}
        (map
          #(field-mapping (option-map (.keyword %)) %)
          (vo/field-selective-seq vo (:only option-map) (:exclude option-map))))
    }))

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

(defn encode-enum [^prime.types.EnumValue in ^JsonGenerator out]
  (.writeStartObject out)
  (if (not (.isInstance scala.Product in))
    (.writeNumberField out "v", (.value in))
    #_else
    (.writeObjectField out "x", (.toString in)))
  (.writeEndObject out))

(defn encode-vo [^prime.vo.ValueObject vo ^JsonGenerator out]
  (.writeStartObject out)
  (doseq [[k v] vo]
    (.writeFieldName out (name k))
    (cheshire.generate/generate out v cheshire.factory/default-date-format nil))
  (.writeEndObject out))

(defn encode-instant [^org.joda.time.ReadableInstant in ^JsonGenerator out]
  (.writeNumber out (.getMillis in)))

(doseq [add-encoder [cheshire.generate/add-encoder, cheshire.custom/add-encoder]]
  (add-encoder prime.types.EnumValue         encode-enum)
  (add-encoder prime.vo.ValueObject          encode-vo)
  (add-encoder org.joda.time.ReadableInstant encode-instant))


;
; ElasticSearch querying API
;

(def-valuesource ElasticSearch-ValueSource [^java.util.Map jmap, ^org.elasticsearch.action.ActionResponse response]
  (contains [this, name idx]          (.containsKey jmap (Integer/toHexString (bit-shift-right idx 8))))
  (anyAt    [this, name idx notFound] (or
    (if jmap
      (let [item (.get jmap (Integer/toHexString (bit-shift-right idx 8)))]
        (if (instance? java.util.Map item)
          (let [item ^java.util.Map item]
            (if-let [x (.get item "x")] x
            (if-let [v (.get item "v")] v
            (ElasticSearch-ValueSource. item, response))))
        #_else  item)))
    notFound)))

(defn voseq-keyfn [^prime.vo.ValueObjectField field]
  (Integer/toHexString (.id field)))


(defn get
  "options: see clj-elasticsearch.client/get-doc"
  [es options]
  (let [resp ^org.elasticsearch.action.get.GetResponse
        (ces/get-doc es (assoc options :format :java))]
    (ElasticSearch-ValueSource. (.sourceAsMap resp) resp)))

(defn put
  "options: see clj-elasticsearch.client/index-doc"
  [es ^ValueObject vo & {:as options :keys [index]}]
  (assert index ":index required")
  (assert vo    "vo required")
  (ces/index-doc es (conj options {
    :id     (.. ^ID vo _id toString)
    :type   (Integer/toHexString (.. vo voManifest ID))
    :source (binding [prime.vo/*voseq-key-fn* voseq-keyfn] (json/encode-smile vo))
  })))
