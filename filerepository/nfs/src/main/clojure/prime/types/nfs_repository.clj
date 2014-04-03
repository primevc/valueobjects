;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.types.nfs-repository
  (:require [prime.types.repository-util :refer (mk-repository)]
            [clojure.java.io :refer (as-file)]))


(defmethod mk-repository "nfs"
  [_ descriptor]
  (let [[_ repository-name mounts-root-path] (re-matches #"^(.+?)@(.+?)$" descriptor)]
    (assert mounts-root-path (str "Missing mount root path in " descriptor "; Example nfs repo uri: nfs://in-docs@/nfs"))
    (prime.types.NFSRepository. (as-file mounts-root-path) repository-name)))
