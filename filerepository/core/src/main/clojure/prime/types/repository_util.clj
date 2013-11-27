;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.types.repository-util
  (:require [taoensso.timbre :as log])
  (:use [clojure.java.io :only [as-file]]
        [prime.types]))


;; FIXME, this cannot be used in this form when libraryized.

;;; File and repository related functions.

(defn local-FileRepository [root-path]
  (prime.types.BasicLocalFileRepository. (as-file root-path)))


(defn local-FileRef [file-or-path]
  (prime.types.LocalFileRef/apply (as-file file-or-path) nil))


(defn local-File [^prime.types.FileRef fileref ^prime.types.LocalFileRepository repository]
  (.getFile repository fileref))


(defn nfs-repository
  "Creates an NFSRepository based on a descriptor string that follows the
  following convention: `<repository-name>@<mounts-root-path>`."
  [descriptor-str]
  (let [[_ repository-name mounts-root-path] (re-matches #"^(.+?)@(.+?)$" descriptor-str)]
    (prime.types.NFSRepository. mounts-root-path repository-name)))


;;; Configuration functions.

(def repository-types #{"basic" "nfs"})

(defn repository
  "Get a repository based on the supplied type, which must be one of those
  defined in `repository-types`, and a descriptor string, such as a path or a
  connection specification. Using this function, bolts can create their own
  reference to a repository based on configuration info they receive."
  [repository-type descriptor-str]
  (assert (repository-types repository-type)
    (apply str "Specified repository type '" repository-type "' is invalid. Please use one of "
      (interpose ", " repository-types)))
  (case repository-type
    "basic" (local-FileRepository descriptor-str)
    "nfs" (nfs-repository descriptor-str)))
