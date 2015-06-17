;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.types.http-repository
  "The implemetation of FileRepository for retrieving (no storing) files using
  the HTTP protocol.

  This repository is not suitable for very large files, as Cassandra may load
  the entire blob into memory.

  Use the functions below when working with the CassandraRepository repository
  from Clojure, as they make the necessary transformations to Scala equivalents.
  For instance, use (store cassrepo #(...)) instead of (.store cassrepo ...).

  The files are retrieved based on the full FileRef.prefixedString, where prefix
  is either http:// or https://."
  (:require [taoensso.timbre :as log]
            [clojure.java.io :as io]
            [clj-http.client :as client]
            [prime.types :refer (to-URI)]
            [prime.types.repository-util :as util])
  (:import [prime.types FileRef]
           [java.io File FileFilter]))


;;; Helper definitions.

(gen-class
  :name "prime.types.HttpRepository"
  :implements [prime.types.FileRepository]
  :init init
  :prefix "repo-"
  :state state
  :constructors {[clojure.lang.IPersistentMap] []})


(deftype State [^clojure.lang.IPersistentMap http-options ^String tmp-dir])


(defn- ref-url
  "Get the full FileRef URL, as a String."
  [^FileRef ref]
  (if (.startsWith (.prefix ref) "http")
    (.prefixedString ref)
    (throw (IllegalArgumentException.
            "Cannot get URL from FileRef not having a prefix starting with `http`"))))


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
    (doseq [^File file (.listFiles dir filter)]
      (log/debug "Deleting file" file)
      (.delete file))))


;;; Implementation of prime.types.FileRepository interface.

(defn repo-existsImpl
  "Test whether the specified FileRef exists in the repository."
  [this ^FileRef ref]
  (log/debug "Checking whether FileRef" ref "exists.")
  (let [exists (= (:status (client/head (ref-url ref))) 200)]
    (log/debug "FileRef" ref (if exists "exists." "does not exist (or is inaccessible)."))
    exists))


(defn repo-toURI
  "Create an URI for the specified FileRef."
  [this ^FileRef ref]
  (to-URI (str ref)))


(defn repo-stream
  "Open an InputStream to the file as referenced by the FileRef. Or
  returns nil/null when the ref cannot be found."
  [this ^FileRef ref]
  (let [response (client/get (ref-url ref) {:as :stream})]
    (when (= (:status response) 200)
      (:body response))))


(defn repo-getFile
  "Returns a File containing the data as referenced by the FileRef."
  [^prime.types.HttpRepository this ^FileRef ref]
  (log/info "File requested for FileRef" ref)
  (let [response (client/get (ref-url ref) {:as :stream})]
    (when (= (:status response) 200)
      (let [^State state (.state this)
            tmp (File/createTempFile "http-" ".download" (File. (.tmp-dir state)))]
        (io/copy (:body response) tmp)
        tmp))))


(def repo-exists util/exists?) ; trait method


;;; Generate HttpRepository class, for use in Java and Scala.

(defn repo-init
  "This function is called when the HttpRepository is constructed."
  [options]
  (log/info "Creating HttpRepository object using options" options)
  (let [tmp-dir (System/getProperty "java.io.tmpdir")]
    (clean-dir (File. tmp-dir) "http-" 24)
    [[] (State. options tmp-dir)]))


;;; API functions. Use these to call the repository functions on the
;;; HTTP file repository.

(defn http-repository
  "Create a new instance of the HttpRepository class, given the
  supplied options map. Currently there are no options."
  [options]
  (prime.types.HttpRepository. (into {} options)))


(def toURI repo-toURI)
(def stream repo-stream)
(def get-file repo-getFile)
