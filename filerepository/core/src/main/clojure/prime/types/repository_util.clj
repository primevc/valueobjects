;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.types.repository-util
  (:require [clojure.java.io :refer (as-file)]
            [prime.types])
  (:import [scala Tuple2]
           [scala.collection JavaConversions]
           [prime.types SelectingFileRepository]))


;;; Repository creation functions.

(defmulti mk-repository
  "Creates a FileRepository based on the specified type and descriptor
  Strings."
  (fn [type descriptor] type))


(defmethod mk-repository :default
  [type _]
  (let [supported (keys (dissoc (.getMethodTable mk-repository) :default))]
    (throw (IllegalArgumentException.
            (str "Unknown repository type '" type "'. Supported types are: "
                 (apply str (interpose "," supported)) ".")))))


(defmethod mk-repository "basic"
  [_ descriptor]
  (prime.types.BasicLocalFileRepository. (as-file descriptor)))


;;; Local file and repository related functions.

(defn local-FileRepository [root-path]
  (prime.types.BasicLocalFileRepository. (as-file root-path)))


(defn local-FileRef [file-or-path]
  (prime.types.LocalFileRef/apply (as-file file-or-path) nil))


(defn local-File [^prime.types.FileRef fileref ^prime.types.LocalFileRepository repository]
  (.getFile repository fileref))


(defn exists?
  "Call `exists` on the FileRepository Scala trait."
  [this ^prime.types.FileRef ref]
  (prime.types.FileRepository$class/exists this ref))


(defn selecting-FileRepository
  "Creates a SelectingFileRepository. Example:

  (selecting-FileRepository #\"http.*\" http-repo
                            #\".*\"    local-repo)"
  [& pairs]
  (->> pairs
       (partition 2)
       (map (fn [[k v]] (Tuple2. k v)))
       JavaConversions/asScalaBuffer
       .toList
       SelectingFileRepository.))
