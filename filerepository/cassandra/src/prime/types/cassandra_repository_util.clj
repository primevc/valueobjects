;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.types.cassandra-repository-util
  "Load this namespace in order to be able to create a Cassandra
  repository through the `mk-repository` multi-method. The returned
  repository uses the Alia containium system."
  (:require [prime.types.cassandra-repository :as repo]
            [prime.types.repository-util :refer (mk-repository)]
            [containium.systems :as system]
            [containium.systems.config :as config]
            [containium.systems.cassandra.alia1 :as alia]))


(defmethod mk-repository "cassandra"
  [_ descriptor]
  (let [[_ repository-name host port] (re-matches #"^(.+?)@(.+?):(.+?)$" descriptor)
        _ (assert (and repository-name host port))
        config (config/map-config {:alia {:contact-points [host]
                                          :port (Integer/parseInt port)}})
        alia (system/start (alia/alia1 :alia) {:config config})]
    (repo/cassandra-repository alia :one repository-name)))
