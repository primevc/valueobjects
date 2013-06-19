;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.util.json
  "Namespace for adding encoders to Cheshire for ValueObjects. This
  namespace does not contain any public functions of interest; it just
  needs to be required."
  (:require [prime.vo :as vo]
            [cheshire.generate :refer (add-encoder)])
  (:import [prime.types VORef EnumValue package$ValueType package$ValueTypes$Tdef
            package$ValueTypes$Tarray package$ValueTypes$Tenum]
           [prime.vo IDField ValueObject ValueObjectManifest ValueObjectField ValueObjectCompanion ID]
           [com.fasterxml.jackson.core JsonGenerator]))


;;; Encoders.

(defn- encode-enum [^EnumValue in ^JsonGenerator out]
  (.writeStartObject out)
  (if-not (.isInstance scala.Product in)
    (.writeNumberField out "v", (.value in))
    (.writeObjectField out "x", (.toString in)))
  (.writeEndObject out))

(def ^:dynamic ^:private *vo-baseTypeID* nil)

(defn- encode-vo
  ([^JsonGenerator out ^prime.vo.ValueObject vo ^String date-format ^Exception ex]
     (encode-vo out vo date-format ex (or *vo-baseTypeID* (.. vo voManifest ID))))

  ([^JsonGenerator out ^prime.vo.ValueObject vo ^String date-format ^Exception ex ^Integer baseTypeID]
     (.writeStartObject out)
     (when-not (== baseTypeID (.. vo voManifest ID))
       (.writeNumberField out "t" (.. vo voManifest ID)))

     (doseq [[k v] vo]                  ; Note that prime.vo/*voseq-key-fn* is used here.
       (.writeFieldName out k)
       (cond
        (instance? ValueObject v)
        ;; First try to find and call a protocol implementation for this type immediately.
        (if-let [to-json (:to-json (clojure.core/find-protocol-impl cheshire.generate/JSONable v))]
          (to-json v out)
          ;; else: Regular VO, no protocol found
          (encode-vo out v date-format ex (.. ^package$ValueTypes$Tdef (. k valueType)
                                              empty voManifest ID)))

        (vector? v)
        (let [innerType (. ^package$ValueTypes$Tarray (. k valueType) innerType)
              voType (if (instance? package$ValueTypes$Tdef innerType)
                       (.. ^package$ValueTypes$Tdef innerType empty voManifest ID)
                       -1)]
          (if (> voType 0)
            (binding [*vo-baseTypeID* voType] (cheshire.generate/generate-array out v date-format ex nil))
            (cheshire.generate/generate-array out v date-format ex nil)))

        :else
        (cheshire.generate/generate out v date-format ex nil)))

     (.writeEndObject out)))

(defn- encode-voref
  [^JsonGenerator out ^VORef in ^String date-format ^Exception ex]
  (cheshire.generate/generate out (._id in) date-format ex nil))

(defn- encode-instant
  [^org.joda.time.ReadableInstant in ^JsonGenerator out]
  (.writeNumber out (.getMillis in)))

(defn- encode-uri
  [^java.net.URI in ^JsonGenerator out]
  (.writeString out (.toString in)))

(defn- encode-url
  [^java.net.URL in ^JsonGenerator out]
  (.writeString out (.toString in)))

(defn- encode-internetAddress
  [^javax.mail.internet.InternetAddress in ^JsonGenerator out]
  (.writeString out (.toString in)))

(defn- encode-objectId
  [^org.bson.types.ObjectId in ^JsonGenerator out]
  (.writeString out (.toString in)))


;;; Register encoders and advice cheshire.generate/generate.

(add-encoder prime.types.EnumValue encode-enum)
(add-encoder java.net.URI encode-uri)
(add-encoder java.net.URL encode-url)
(add-encoder org.joda.time.ReadableInstant encode-instant)
(add-encoder org.bson.types.ObjectId encode-objectId)
(add-encoder javax.mail.internet.InternetAddress encode-internetAddress)

(alter-var-root
 #'cheshire.generate/generate
 (fn [orig-generate]
   (fn [^JsonGenerator jg obj ^String date-format ^Exception ex key-fn]
     ;; First try to find and call a protocol implementation for this type immediately.
     (if-let [to-json (:to-json (clojure.core/find-protocol-impl cheshire.generate/JSONable obj))]
       (to-json obj jg)
       ;; else: No protocol found
       (condp instance? obj
         ValueObject (encode-vo jg obj date-format ex)
         VORef (encode-voref jg obj date-format ex)
         (orig-generate jg obj date-format ex key-fn))))))
