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
  (:require [containium.systems.cassandra :as cassandra]
            [taoensso.timbre :as log]
            [clojure.java.io :as io])
  (:import [prime.types FileRef LocalFileRef FileRefInputStream FileRefOutputStream]
           [java.io File InputStream OutputStream ByteArrayOutputStream FileInputStream FileFilter]
           [java.nio ByteBuffer MappedByteBuffer]
           [java.nio.channels FileChannel FileChannel$MapMode]
           [java.nio.file StandardOpenOption]
           [org.apache.commons.codec.binary Base64]
           [org.apache.cassandra.utils ByteBufferUtil]))


;;;---TODO: See how the optimisations of the absorb and stream help, and how
;;;         they can be improved.


;;; Helper definitions.

(defn- prepare-statements
  "Prepare the statements used for the Cassandra repository."
  [system consistency]
  (log/debug "Preparing statements for Cassandra repository.")
  (letfn [(mk-cassandra-fn [statement]
            (let [prepared (cassandra/prepare system statement)]
              (partial cassandra/do-prepared system prepared {:consistency consistency
                                                              :keywordize? true})))]
    {:create (mk-cassandra-fn "INSERT INTO files (hash, data) VALUES (?, ?);")
     :exists (mk-cassandra-fn "SELECT COUNT(*) FROM files WHERE hash=?;")
     :stream (mk-cassandra-fn "SELECT data FROM files WHERE hash=?;")
     :delete (mk-cassandra-fn "DELETE FROM files WHERE hash=?;")}))


(defn- ref-hash
  "Evaluates to the base64 encoded hash of the specified FileRef."
  [^FileRef ref]
  (Base64/encodeBase64URLSafeString (.hash ref)))


(defn- write-schema
  "This function writes the schema for the Cassandra repository, using
  the supplied cluster."
  [system]
  (when-not (cassandra/has-keyspace? system "fs")
    (log/info "Writing schema to Cassandra.")
    (cassandra/write-schema system (slurp (resource "cassandra-repo-schema.cql")))))


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
            statement-fn (-> state :statements :create)
            hash (ref-hash ref)]
        (log/info "Storing file using MappedByteBuffer for FileRef" ref)
        (statement-fn [hash buffer])))
    ref))


(defn- write-bytes
  [repo ref byte-array]
  (if-not (exists repo ref)
    (let [state (.state repo)
          statement-fn (-> state :statements :create)
          buffer (ByteBuffer/wrap byte-array)
          hash (ref-hash ref)]
      (log/debug "Underlying FileRef not stored yet, storing it now.")
      (statement-fn [hash buffer]))
    (log/debug "Underlying FileRef already stored.")))


(defn- clean-dir
  "Removes files from dir, having a name starting with prefix (can be
  nil), and was not modified the last specified hours."
  [^File dir ^String prefix ^Long hours]
  (log/info "Removing files from" (.getAbsolutePath dir) "having a the prefix" prefix
            "and not modified since the last" hours "hours.")
  (let [filter (reify FileFilter
                 (^boolean accept [this ^File f]
                   (boolean (and (.isFile f)
                                 (if prefix (.. f getName (startsWith prefix)) true)
                                 (< (.lastModified f)
                                    (- (System/currentTimeMillis) (* 1000 60 60 hours)))))))]
    (doseq [file (.listFiles dir filter)]
      (log/debug "Deleting file" file)
      (.delete file))))


;;; Implementation of prime.types.FileRepository interface.

(defn repo-existsImpl
  "Test whether the specified FileRef exists in the repository."
  [this ^FileRef ref]
  (log/info "Checking whether FileRef" ref "is stored.")
  (let [state (.state this)
        statement-fn (-> state :statements :exists)
        hash (ref-hash ref)
        result (statement-fn [hash])
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
        statement-fn (-> state :statements :stream)
        hash (ref-hash ref)
        result (statement-fn [hash])
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


(defn repo-getFile
  "Returns a File containing the data as referenced by the FileRef."
  [this ^FileRef ref]
  (log/info "File requested for FileRef" ref)
  (let [state (.state this)
        hash (ref-hash ref)
        tmp (File. (:tmp-dir state) (str "cfr-" hash))]
    (if-not (.exists tmp)
      (do (log/info "File not available in temporary directory; creating it now.")
          (let [statement-fn (-> state :statements :stream)
                result (statement-fn [hash])
                data (:data (first result))
                channel (FileChannel/open (.toPath tmp) (into-array [StandardOpenOption/CREATE_NEW
                                                                     StandardOpenOption/WRITE]))]
            (.write channel data)
            (.close channel)))
      (do (log/info "File already available in temporary directory; touching it now.")
          (.setLastModified tmp (System/currentTimeMillis)))) ;; touch
    tmp))


(defn repo-delete
  "Deletes the specified FileRef from the repository. This function is called by
  the garbage collector and should not be called by other clients."
  [this ^FileRef ref]
  (log/info "Deleting file for FileRef" ref)
  (let [state (.state this)
        statement-fn (-> state :statements :delete)
        hash (ref-hash ref)]
    (statement-fn [hash])))


;;; Generate CassandraRepository class, for use in Java and Scala.

(defn repo-init
  "This function is called when the CassandraRepository is constructed."
  [system consistency repository-name]
  (log/info "Creating CassandraRepository object for repository" repository-name)
  (write-schema system)
  (clean-dir (File. (System/getProperty "java.io.tmpdir")) "cfr-" 24)
  (let [session (cassandra/keyspaced system "fs")
        statement-fns (prepare-statements session consistency)]
    [[] {:repository-name repository-name
         :statements statement-fns
         :prefix "cassandra://"
         :tmp-dir (System/getProperty "java.io.tmpdir")}]))


(gen-class
  :name "prime.types.CassandraRepository"
  :implements [prime.types.GarbageCollectableFR]
  :init init
  :prefix "repo-"
  :state state
  :constructors {[Object clojure.lang.Keyword String] []})


;;; API functions. Use these to call the repository functions on the
;;; Cassandra file repository.

(defn cassandra-repository
  "Create a new instance of the CassandraRepository class, given the
  supplied Cassandra containium system, the consistency keyword to use
  for queries and the repository name."
  [system consistency repository-name]
  (prime.types.CassandraRepository. system consistency repository-name))


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
(def get-file repo-getFile)
