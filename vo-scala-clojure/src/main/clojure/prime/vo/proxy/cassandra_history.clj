;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.proxy.cassandra-history
  "A concrete implementation of the VOProxy and VOHistoryProxy
  protocols, having Cassandra as its backend."
  (:require [prime.vo.proxy :refer (VOProxy VOHistoryProxy)]
            [prime.vo.util.cassandra-history :as ch]))

(deftype CassandraHistoryVOProxy [cluster]
  VOProxy
  (get-vo [this vo]
    (ch/get cluster vo))

  (put-vo [this vo]
    (ch/put cluster vo {}))

  (put-vo [this vo options]
    (ch/put cluster vo options))

  (update [this vo id]
    (ch/update cluster vo id {}))

  (update [this vo id options]
    (ch/update cluster vo id options))

  (delete [this vo]
    (ch/delete cluster vo {}))

  (delete [this vo options]
    (ch/delete cluster vo options))

  (append-to [this vo path path-vars value]
    (ch/append-to cluster vo path path-vars value {}))

  (append-to [this vo path path-vars value options]
    (ch/append-to cluster vo path path-vars value options))

  (insert-at [this vo path path-vars value]
    (ch/insert-at cluster vo path path-vars value {}))

  (insert-at [this vo path path-vars value options]
    (ch/insert-at cluster vo path path-vars value options))

  (move-to [this vo path path-vars to]
    (ch/move-to cluster vo path path-vars to {}))

  (move-to [this vo path path-vars to options]
    (ch/move-to cluster vo path path-vars to options))

  (replace-at [this vo path path-vars value]
    (ch/replace-at cluster vo path path-vars value {}))

  (replace-at [this vo path path-vars value options]
    (ch/replace-at cluster vo path path-vars value options))

  (merge-at [this vo path path-vars value]
    (ch/merge-at cluster vo path path-vars value {}))

  (merge-at [this vo path path-vars value options]
    (ch/merge-at cluster vo path path-vars value options))

  (remove-from [this vo path path-vars]
    (ch/remove-from cluster vo path path-vars {}))

  (remove-from [this vo path path-vars options]
    (ch/remove-from cluster vo path path-vars options))

  VOHistoryProxy
  (get-slice [this vo]
    (ch/get-slice cluster vo {}))

  (get-slice [this vo options]
    (ch/get-slice cluster vo options))
  )
