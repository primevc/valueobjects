;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.types.cassandra-util
  "API functions for using the embedded Cassandra instance from
  Clojure."
  (:require [clojure.java.io :refer (copy)])
  (:import [org.apache.cassandra.cql3 QueryProcessor UntypedResultSet]
           [org.apache.cassandra.db ConsistencyLevel]
           [org.apache.cassandra.service ClientState QueryState]
           [org.apache.cassandra.transport.messages ResultMessage$Rows]
           [org.apache.cassandra.utils ByteBufferUtil]
           [java.io ByteArrayOutputStream]
           [java.util Arrays]
           [java.nio CharBuffer ByteBuffer]
           [java.nio.charset Charset]))


;;; Helper functions.

(defn- ->bytebuffer
  "Converts some basic type to a ByteBuffer. Currently supported types
  are: String, Long, byte array and ByteBuffer."
  [primitive]
  (condp instance? primitive
    String
    (let [encoder (.newEncoder (Charset/forName "UTF-8"))]
      (.encode encoder (CharBuffer/wrap ^String primitive)))

    (Class/forName "[B")
    (ByteBuffer/wrap primitive)

    Long
    (.putLong (ByteBuffer/allocate 8) primitive)

    ByteBuffer
    primitive))


(defn bytebuffer->inputstream
  "Returns an InputStream reading from a ByteBuffer."
  [^ByteBuffer bb]
  (ByteBufferUtil/inputStream bb))


(defn bytebuffer->bytes
  "Converts a ByteBuffer to a byte array. If the ByteBuffer is backed
  by an array, a copy of the relevant part of that array is returned.
  Otherwise, the bytes are streamed into a byte array."
  [^ByteBuffer bb]
  (if (.hasArray bb)
    (Arrays/copyOfRange (.array bb)
                        (+ (.position bb) (.arrayOffset bb))
                        (+ (.position bb) (.arrayOffset bb) (.limit bb)))
    (let [baos (ByteArrayOutputStream. (.remaining bb))]
      (copy (bytebuffer->inputstream bb) baos)
      (.toByteArray baos))))


;;; General Cassandra functions.

(def ^:private client-state (ClientState. true))
(def ^:private query-state (QueryState. client-state))


(defn prepare
  "Create a prepared CQLStatement."
  [query-str]
  (-> query-str
      (QueryProcessor/prepare client-state false)
      .statementId
      QueryProcessor/getPrepared))


(defn do-prepared
  "Execute a prepared CQLStatement, using the specified consistency (a
  keyword) and the positional arguments for the prepared query. If
  rows are returned by the query, an UntypedResultSet is returned,
  otherwise nil."
  [pp-query consistency & args]
  (let [consistency (case consistency
                      :any ConsistencyLevel/ANY
                      :one ConsistencyLevel/ONE
                      :two ConsistencyLevel/TWO
                      :three ConsistencyLevel/THREE
                      :quorum ConsistencyLevel/QUORUM
                      :all ConsistencyLevel/ALL
                      :local-quorum ConsistencyLevel/LOCAL_QUORUM
                      :each-quorum ConsistencyLevel/EACH_QUORUM)
        result (QueryProcessor/processPrepared pp-query consistency query-state
                                               (map ->bytebuffer args))]
    (when (instance? ResultMessage$Rows result)
      (UntypedResultSet. (.result ^ResultMessage$Rows result)))))
