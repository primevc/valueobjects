;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.types.kafka-util
  (:require [taoensso.timbre :as log])
  (:use [clojure.stacktrace])
  (:import java.util.Properties
           [kafka.javaapi.producer Producer ProducerData]
           [kafka.producer ProducerConfig]))


;;; Kafka related definitions

(defn producer-config
  "Given the host:port String of how to connect to Zookeeper, this function
  returns a ProducerConfig that can be used to send messages to Kafka."
  [zk-connect-str]
  (-> (doto (Properties.)
        (.put "serializer.class" "nl.storm.MessagePackVOSerializer")
        (.put "zk.connect" zk-connect-str))
      (ProducerConfig.)))


(defn send-message-to-kafka
  "A function to send a message to Kafka."
  [producer-config topic message]
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

(comment
  "This is the stateful way of doing it. It depends on whether we see Shared as
  purely a library or not. If seen as a library, then the functional implementation
  above should be used, as it is up to the clients of a library to decide on how to
  store state. If we see Shared as part of our application, the stateful approach
  below is fine."

  (def producer (atom nil))

  (defn configure-producer
    "Given the host:port String of how to connect to Zookeeper, this function
    initialises this namespace in order to send messages to Kafka."
    [zk-connect-str]
    (->> (doto (Properties.)
           (.put "serializer.class" "nl.storm.MessagePackVOSerializer")
           (.put "zk.connect" zk-connect-str))
         (ProducerConfig.)
         (Producer.)
         (reset! producer)))


  (defn send-message-to-kafka
    "A function to send a message to Kafka. Make sure `configure-producer` has
    been called at least once."
    [topic message]
    (let [^ProducerData producer-data (ProducerData. topic message)]
      (try
        (log/info "Sending" (class message) "to topic:" topic)
        (.send @producer producer-data)
        (log/info "Sending done.")
        (catch Exception e
          (log/error "Error upon trying to send message" message "to topic" topic "-" (.getMessage e)
                     "\n" (with-out-str (print-cause-trace e))))
        (finally
          (.close producer))))))


;;; Kafka related constants.

(def ^:const conversions-topic "conversions-topic")
(def ^:const postback-topic "postback-topic")
