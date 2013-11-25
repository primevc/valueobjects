;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.util.msgpack
  (:refer-clojure :exclude [get])
  (:require 
    [prime.vo :as vo]
    [clojure.java.io :as io])
  (:import 
    [prime.utils.msgpack.VOPacker]
    [prime.utils.msgpack.VOUnpacker]
    [java.io File FileOutputStream FileInputStream]
    [prime.vo ValueObject]
    [org.msgpack.Unpacker]
    )
  )

(defn make-dirs [& directory]
  (let [directory (java.io.File. (apply str directory))]
    (when-not (.isDirectory directory)
      (.mkdir directory))
    (.toString directory)))

(defn create-filename [directory vo & {:keys [appendix id]}]
  (apply str (make-dirs directory "/" (.. vo voManifest ID)) "/" appendix (or id (:id vo)) ".vo.gz"))

(defn create-tmp-filename [directory vo]
  (create-filename directory vo :appendix (str "tmp-" (rand 1000) "-" (.getId (Thread/currentThread)) "-")))

(defn read-file [filename])

(defn rename-file "This is used to avoid multithread overwriting." [filename new-fn]
  (.renameTo (java.io.File. filename) (java.io.File. new-fn)))

(defn execute [& command]
  (let [process (.exec (Runtime/getRuntime) (apply str command))]
    process))

(defn pack-to-tmp-gzip
  "Create a package to temporary gzip file. Returns the created java.io.File."
  [vo ^File file-or-directory]
  (let [temp-file  (if-not (.isDirectory file-or-directory) file-or-directory
                    #_else (create-tmp-filename file-or-directory vo))
        sevenzip   (execute "7za a "temp-file" -tgzip -mx=9 -si")
        zip-stream (.getOutputStream sevenzip)
        msgpack    (prime.utils.msgpack.VOPacker. zip-stream)]
    (.pack msgpack vo)
    (.close zip-stream)
    (.waitFor sevenzip)
    (io/as-file temp-file)))

(defn pack
  "Create package. Write it to a temp file. Rename temp file."
  [vo directory]
  (rename-file (pack-to-tmp-gzip vo directory) (create-filename directory vo)))

(defn unpack [vo directory]
  (if-not (.exists (java.io.File. (create-filename directory vo)))
    nil
    (let [  unpacker    (org.msgpack.Unpacker. (java.util.zip.GZIPInputStream. (java.io.FileInputStream. (create-filename directory vo))))]
      (.setVOHelper unpacker prime.utils.msgpack.VOUnpacker$/MODULE$)
      (.. vo voCompanion (valueOf (.next unpacker))))))

(defn get [directory ^ValueObject vo] 
  (assert (prime.vo/has-id? vo) (str "vo: " (prn-str vo) " requires an id"))
  (unpack vo directory))

(defn put [directory ^ValueObject vo options]
  (assert (prime.vo/has-id? vo) (str "vo: " (prn-str vo) " requires an id"))
  (pack vo directory))

(defn delete [directory ^ValueObject vo options]
  (.delete (java.io.File. (create-filename directory vo))))

(defn update [directory ^ValueObject vo id options]
  (let [old-vo (get directory (conj vo {:id id}))] ; Conj id into vo to overwrite possible new id.
    (when (not= id (:id vo)) ; Delete old file
      (delete directory old-vo {}))
    (put directory (if old-vo (conj old-vo vo) vo) options)))
