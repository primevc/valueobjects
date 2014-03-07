;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.proxy.elasticsearch
  "A concrete implementation of the VOProxy and VOSearchProxy
  protocols, with ElasticSearch as the backend."
  (:require [prime.vo.proxy :refer (VOProxy VOSearchProxy)]
            [prime.vo.util.elasticsearch :as es]))


(defrecord ElasticSearchVOProxy
    [^String index
     ^org.elasticsearch.client.transport.TransportClient client
     default-opts] ;; Supported: :refresh-at-change

  VOProxy
  (get-vo [this vo]
    (es/get this vo {}))

  (put-vo [this vo]
    (es/put this vo {}))

  (put-vo [this vo options]
    (es/put this vo options))

  (update [this vo]
    (es/update this vo (:id vo) {}))

  (update [this vo id]
    (es/update this vo id {}))

  (update [this vo id options]
    (es/update this vo id options))

  (delete [this vo]
    (es/delete this vo {}))

  (delete [this vo options]
    (es/delete this vo options))

  (append-to [this vo path path-vars value]
    (es/append-to this vo path (or path-vars ()) value {}))

  (append-to [this vo path path-vars value options]
    (es/append-to this vo path (or path-vars ()) value options))

  (insert-at [this vo path path-vars value]
    (es/insert-at this vo path (or path-vars ()) value {}))

  (insert-at [this vo path path-vars value options]
    (es/insert-at this vo path (or path-vars ()) value options))

  (move-to [this vo path path-vars to]
    (es/move-to this vo path (or path-vars ()) to {}))

  (move-to [this vo path path-vars to options]
    (es/move-to this vo path (or path-vars ()) to options))

  (replace-at [this vo path path-vars value]
    (es/replace-at this vo path (or path-vars ()) value {}))

  (replace-at [this vo path path-vars value options]
    (es/replace-at this vo path (or path-vars ()) value options))

  (merge-at [this vo path path-vars value]
    (es/merge-at this vo path (or path-vars ()) value {}))

  (merge-at [this vo path path-vars value options]
    (es/merge-at this vo path (or path-vars ()) value options))

  (remove-from [this vo path path-vars]
    (es/remove-from this vo path (or path-vars ()) {}))

  (remove-from [this vo path path-vars options]
    (es/remove-from this vo path (or path-vars ()) options))

  VOSearchProxy
  ;; [es ^ValueObject vo indices & {:as options :keys [ query filter from size types sort
  ;; highlighting only exclude script-fields preference facets named-filters boost explain version
  ;; min-score listener ignore-indices routing listener-threaded? search-type operation-threading
  ;; query-hint scroll source]}]
  (search [this vo]
    (es/search this vo {}))

  (search [this vo options]
    (es/search this vo options)))
