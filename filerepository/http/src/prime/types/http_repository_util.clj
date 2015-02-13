;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.types.http-repository-util
  "Load this namespace in order to be able to create a HTTP
  repository through the `mk-repository` multi-method."
  (:require [prime.types.http-repository :as repo]
            [prime.types.repository-util :refer (mk-repository)]))


(defmethod mk-repository "http"
  [_ descriptor]
  (repo/http-repository {}))
