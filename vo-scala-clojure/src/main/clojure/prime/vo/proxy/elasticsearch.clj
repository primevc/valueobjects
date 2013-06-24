;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.proxy.elasticsearch
  "A concrete implementation of the VOProxy and VOSearchProxy
  protocols, with ElasticSearch as the backend."
  (:require [prime.vo.proxy :refer (VOProxy VOSearchProxy)]
            [prime.vo.util.elasticsearch :as es]))


(deftype ElasticSearchVOProxy [^String index ^org.elasticsearch.client.transport.TransportClient client]

  VOProxy
  ;; [es es-index ^ValueObject vo options]
  (get-vo [this vo]
    (es/get client index vo))

  (put-vo [this vo]
    (es/put client index vo {}))

  (put-vo [this vo options]
    (es/put client index vo options))

  (update [this vo id]
    (es/update client index vo id {}))

  (update [this vo id options]
    (es/update client index vo id options))

  (delete [this vo]
    (es/delete client index vo {}))

  (delete [this vo options]
    (es/delete client index vo options))

  (append-to [this vo path path-vars value]
    (es/append-to client index vo path (or path-vars ()) value {}))

  (append-to [this vo path path-vars value options]
    (es/append-to client index vo path (or path-vars ()) value options))

  (insert-at [this vo path path-vars value]
    (es/insert-at client index vo path (or path-vars ()) value {}))

  (insert-at [this vo path path-vars value options]
    (es/insert-at client index vo path (or path-vars ()) value options))

  (move-to [this vo path path-vars to]
    (es/move-to client index vo path (or path-vars ()) to {}))

  (move-to [this vo path path-vars to options]
    (es/move-to client index vo path (or path-vars ()) to options))

  (replace-at [this vo path path-vars value]
    (es/replace-at client index vo path (or path-vars ()) value {}))

  (replace-at [this vo path path-vars value options]
    (es/replace-at client index vo path (or path-vars ()) value options))

  (merge-at [this vo path path-vars value]
    (es/merge-at client index vo path (or path-vars ()) value {}))

  (merge-at [this vo path path-vars value options]
    (es/merge-at client index vo path (or path-vars ()) value options))

  (remove-from [this vo path path-vars]
    (es/remove-from client index vo path (or path-vars ()) {}))

  (remove-from [this vo path path-vars options]
    (es/remove-from client index vo path (or path-vars ()) options))

  VOSearchProxy
  ;; [es ^ValueObject vo indices & {:as options :keys [ query filter from size types sort highlighting only exclude script-fields preference facets named-filters boost explain version min-score listener ignore-indices routing listener-threaded? search-type operation-threading query-hint scroll source]}]
  (search [this vo]
    (es/search client index vo {}))

  (search [this vo options]
    (es/search client index vo options))

  )
