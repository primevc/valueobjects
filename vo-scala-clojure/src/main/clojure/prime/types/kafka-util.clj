;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.types.kafka-util
  (:require [taoensso.timbre :as log])
  (:use [clojure.stacktrace])
  (:import java.util.Properties
           [kafka.consumer ConsumerConfig ConsumerIterator ConsumerConnector]
           [kafka.javaapi.producer Producer ProducerData]
           [kafka.producer ProducerConfig]))


;;; Kafka related definitions

(def ^:private props (doto (Properties.)
                  ;(.put "producer.type" "async")
                  (.put "serializer.class" "nl.storm.MessagePackVOSerializer")
                  (.put "zk.connect" "127.0.0.1:2181")))

(def ^:private producer-config (ProducerConfig. props))


(defn send-message-to-kafka
  "A function to send a message to Kafka."
  [topic message]
  (let [^ProducerData producer-data (ProducerData. topic message)
        ^Producer producer (Producer. producer-config)]
    (try
      (log/info "Sending" (class message) "to topic:" topic)
      (.send producer producer-data)
      (log/info "Sending done.")
      (catch Exception e
        (log/error "Error upon trying to send message" message "to topic" topic "-" (.getMessage e)
                   "\n" (with-out-str (print-cause-trace e))))
      (finally
        (.close producer)))))


;;; Kafka related constants.

(def ^:const conversions-topic "conversions-topic")
(def ^:const postback-topic "postback-topic")
