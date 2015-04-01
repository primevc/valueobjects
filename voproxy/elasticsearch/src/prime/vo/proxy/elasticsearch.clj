;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.proxy.elasticsearch
  "A concrete implementation of the VOProxy and VOSearchProxy
  protocols, with ElasticSearch as the backend."
  (:import [org.elasticsearch.node Node])
  (:require [prime.vo.proxy :refer (VOProxy VOSearchProxy)]
            [clojurewerkz.elastisch.native.index :refer (delete-mapping)]
            [prime.vo.util.elasticsearch :as es]))


(defrecord ElasticSearchVOProxy
    [^String index
     ^org.elasticsearch.client.Client client
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
    (es/update this vo (or id (:id vo)) {}))

  (update [this vo id options]
    (es/update this vo (or id (:id vo)) (or options {})))

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

(defn ->ElasticSearchVOProxy
  ([index node-or-client]
     (ElasticSearchVOProxy. index node-or-client {}))
  ([index node-or-client default-opts]
     (ElasticSearchVOProxy. index
                            (if (instance? Node node-or-client)
                              (.client ^Node node-or-client)
                             #_else node-or-client)
                            default-opts)))


(defn reindex
  "Usage:
    (reindex foo-proxy \"foo-index\" setup/foo-mapping
             (->> (history/get-ids foo-history) (map #(get-vo foo-history %))))"
  [target-voproxy idx-name mapping seq-of-new-vos]
  (when-let [first-vo (->> seq-of-new-vos (remove nil?) (first))]
    (delete-mapping  (:client target-voproxy) idx-name (es/vo-hexname first-vo))
    (es/create-index (:client target-voproxy) idx-name mapping)
    (doseq [vo seq-of-new-vos :when vo]
      (prime.vo.proxy/put-vo target-voproxy vo))))
