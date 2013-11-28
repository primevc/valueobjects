;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.types.cassandra-repository
  "The implemetation of FileRepository for storing and retrieving files using
  the distributed database Cassandra.

  This repository is not suitable for very large files, as Cassandra may load
  the entire blob into memory.

  Use the functions below when working with the CassandraRepository repository
  from Clojure, as they make the necessary transformations to Scala equivalents.
  For instance, use (store cassrepo #(...)) instead of (.store cassrepo ...).

  This repository has the following URI convention: `cassandra://<hash>`"
  (:refer-clojure :exclude (replace))
  (:use [prime.types :only (to-URI)]
        [clojure.java.io :only (resource)]
        [clojure.string :only (replace split)])
  (:require [qbits.alia :as alia]
            [taoensso.timbre :as log]
            [clojure.java.io :as io])
  (:import [prime.types FileRef LocalFileRef FileRefInputStream FileRefOutputStream]
           [java.io File InputStream OutputStream ByteArrayOutputStream FileInputStream]
           [java.nio ByteBuffer MappedByteBuffer]
           [java.nio.channels FileChannel FileChannel$MapMode]
           [org.apache.commons.codec.binary Base64]
           [org.apache.cassandra.utils ByteBufferUtil]))


;;;---TODO: See how the optimisations of the absorb and stream help, and how
;;;         they can be improved.


;;; Helper definitions.

;; The consistency to use for the queries. One can set this to another value
;; when testing, for example.
(def consistency (atom :quorum))


(defn- prepare-statements
  "Prepare the statements used for the Cassandra repository."
  [session]
  (log/debug "Preparing statements for Cassandra repository.")
  {:create (alia/prepare session "INSERT INTO files (hash, data) VALUES (?, ?);")
   :exists (alia/prepare session "SELECT COUNT(*) FROM files WHERE hash=?;")
   :stream (alia/prepare session "SELECT data FROM files WHERE hash=?;")
   :delete (alia/prepare session "DELETE FROM files WHERE hash=?;")})


(defn- ref-hash
  "Evaluates to the base64 encoded hash of the specified FileRef."
  [^FileRef ref]
  (Base64/encodeBase64URLSafeString (.hash ref)))


(defn- cql-statements
  "Returns the CQL statements from the specified String in a sequence."
  [s]
  (let [no-comments (-> s
                        (replace #"(?s)/\*.*?\*/" "")
                        (replace #"--.*$" "")
                        (replace #"//.*$" ""))]
    (map #(str % ";") (split no-comments #"\s*;\s*"))))


(defn write-schema
  "This function writes the schema for the Cassandra repository, using
  the supplied cluster."
  [cluster]
  (log/info "Writing schema to Cassandra.")
  (let [session (alia/connect cluster)
        schema (slurp (resource "cassandra-repo-schema.cql"))]
    (log/debug "Schema to write: " schema)
    (doseq [s (cql-statements schema)]
      (log/debug "Executing statement:" s)
      (alia/execute session s))))


;;---TODO: Play with this when we get actual metrics.
(def ^:const memory-map-minimum 1048576) ; 1 MiB

(declare exists)

(defn- memory-mapped-absorb
  "Absorbs the given File using a MappedByteBuffer. This improves the memory
  efficiency when absorbing large files."
  [repo ^File file]
  (let [state (.state repo)
        prefix (:prefix state)
        ref (LocalFileRef/apply file prefix)]
    (if (exists repo ref)
      (log/info "File already stored for FileRef" ref)
      (let [fis (FileInputStream. file)
            fc (.getChannel fis)
            buffer (.map fc (FileChannel$MapMode/READ_ONLY) 0 (.size fc))
            session (:session state)
            statement (-> state :statements :create)
            hash (ref-hash ref)]
        (log/info "Storing file using MappedByteBuffer for FileRef" ref)
        (alia/execute session statement :values [hash buffer] :consistency @consistency)))
    ref))


(defn- write-bytes
  [repo ref byte-array]
  (if-not (exists repo ref)
    (let [state (.state repo)
          session (:session state)
          statement (-> state :statements :create)
          buffer (ByteBuffer/wrap byte-array)
          hash (ref-hash ref)]
      (log/debug "Underlying FileRef not stored yet, storing it now.")
      (alia/execute session statement :values [hash buffer] :consistency @consistency))
    (log/debug "Underlying FileRef already stored.")))


;;; Implementation of prime.types.FileRepository interface.

(defn repo-existsImpl
  "Test whether the specified FileRef exists in the repository."
  [this ^FileRef ref]
  (log/info "Checking whether FileRef" ref "is stored.")
  (let [state (.state this)
        session (:session state)
        statement (-> state :statements :exists)
        hash (ref-hash ref)
        result (alia/execute session statement :values [hash] :consistency @consistency)
        exists (= 1 (:count (first result)))]
    (log/info "FileRef" ref (if exists "is stored." "is not stored."))
    exists))


(defn repo-toURI
  "Create an URI for the specified FileRef."
  [this ^FileRef ref]
  (to-URI (str ref)))


(defn repo-absorb
  "Absorb the content given by the specified input. The input can be of the
  following types:

  - java.io.IntputStream: Absorbs the entire inputstream, after which it is
                          closed.

  - java.io.File: Absorb the specified File into the repository. The File is
                  deleted after it is absorbed.

  A FileRef to the location of the stored content is returned."
  [this in]
  (log/info "Absorb called with inputstream:" in)
  (condp instance? in
    ;; InputStream implementation.
    InputStream
    (let [state (.state this)
          prefix (:prefix state)
          fris (FileRefInputStream. ^InputStream in ^String prefix)
          out (ByteArrayOutputStream.)]
      (io/copy fris out)
      (let [ref (.ref fris)]
        (write-bytes this ref (.toByteArray out))
        ref))
    ;; File implementation.
    File
    (let [^File file in
          size (.length file)]
      (log/debug "File to absorb has size of" size "bytes.")
      (let [ref (if (< size memory-map-minimum)
                  (repo-absorb this (FileInputStream. file))
                  (memory-mapped-absorb this file))]
        (log/debug "Deleting original file.")
        (.delete file)
        ref))))


(defn repo-stream
  "Open an InputStream to the file as referenced by the FileRef."
  [this ^FileRef ref]
  (log/info "Stream requested for FileRef" ref)
  (let [state (.state this)
        session (:session state)
        statement (-> state :statements :stream)
        hash (ref-hash ref)
        result (alia/execute session statement :values [hash] :consistency @consistency)
        data (:data (first result))]
    (ByteBufferUtil/inputStream data)))


(defn repo-store
  "Stores the data given a Scala function that receives an
  OutputStream from the repository. Use the 'store' function below to
  use a Clojure function."
  [this ^scala.Function1 sf]
  (let [state (.state this)
        prefix (:prefix state)
        out (ByteArrayOutputStream.)
        fros (FileRefOutputStream. ^OutputStream out ^String prefix)]
    (.apply sf fros)
    (let [ref (.ref fros)]
      (write-bytes this ref (.toByteArray out))
      ref)))


(defn repo-delete
  "Deletes the specified FileRef from the repository. This function is called by
  the garbage collector and should not be called by other clients."
  [this ^FileRef ref]
  (log/info "Deleting file for FileRef" ref)
  (let [state (.state this)
        session (:session state)
        statement (-> state :statements :delete)
        hash (ref-hash ref)]
    (alia/execute session statement :values [hash] :consistency @consistency)))


;;; Generate CassandraRepository class, for use in Java and Scala.

(defn repo-init
  "This function is called when the CassandraRepository is constructed."
  [cluster repository-name]
  (log/info "Creating CassandraRepository object for repository" repository-name
            "using cluster:" (.. cluster getMetadata getClusterName))
  (let [session (alia/connect cluster "fs")
        statements (prepare-statements session)]
    [[] {:repository-name repository-name
         :session session
         :statements statements
         :prefix "cassandra://"}]))

(gen-class
  :name "prime.types.CassandraRepository"
  :implements [prime.types.GarbageCollectableFR]
  :init init
  :prefix "repo-"
  :state state
  :constructors {[com.datastax.driver.core.Cluster String] []})


;;; API functions. Use these to call the repository functions on the
;;; Cassandra file repository.

(defn cassandra-repository
  "Create a new instance of the CassandraRepository class, given the supplied
  cluster and repository name."
  [cluster repository-name]
  (prime.types.CassandraRepository. cluster repository-name))


(defn exists
  "Call `exists` on the FileRepository Scala trait."
  [this ^FileRef ref]
  (log/debug "Exists called from Clojure, forwarded to Scala trait.")
  (prime.types.FileRepository$class/exists this ref))


(defn store
  "Call `store` on the FileRepository Scala trait. The supplied function `f`
  must be an ordinary Clojure function, which takes a FileRefOutputStream as its
  sole argument."
  [this f]
  (log/info "Store requested with Clojure function.")
  (let [scala-func (reify scala.Function1 (apply [_ x] (f x)))]
    (.store this scala-func)))


(def toURI repo-toURI)
(def absorb repo-absorb)
(def stream repo-stream)
