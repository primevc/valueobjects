;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.storageproxy
  (:require 
    [prime.vo.util.elasticsearch      :as es]
    [prime.vo.util.msgpack            :as mp]
    [prime.vo.util.cassandra-history   :as ch]))

(defprotocol VOProxy
  "WARNING: Proxy cannot guarantee consistency!"
  ; Get
  ; Get will get you a VO based on ID
  (get-vo [vo] [proxy vo])

  ; Put
  ; Putting will replace the existing VO with the new one.
  ; Requires ID
  (put-vo [vo] [proxy vo] [proxy vo options] "Replaces existing VO.")

  ; Update
  ; Updating will add the new VO to the extisting one. Replacing data if it already exists
  (update [vo] [proxy vo] [proxy vo id] [proxy vo id options] "Updates a VO")

  ; Delete
  ; Deletes a VO. Use with care :-)
  (delete [vo] [proxy vo] [proxy vo options])

  ; Update-if
  ; Same as update but only executes when certain requirements are met.
  #_(update-if [proxy predicate options])

  ; VECTOR-BASED
  (appendTo [vo path path-vars value] [this vo path path-vars value] [this vo path path-vars value options])

  (insertAt [vo path path-vars value] [proxy vo path path-vars value] [proxy vo path path-vars value options])

  (moveTo [vo path path-vars to] [proxy vo path path-vars to] [proxy vo path path-vars to options])

  (replaceAt [vo path path-vars value] [proxy vo path path-vars value] [proxy vo path path-vars value options])

  (mergeAt [vo path path-vars value] [prxoy vo path path-vars value] [proxy vo path path-vars value options])

  (removeFrom [vo path path-vars] [proxy vo path path-vars] [proxy vo path path-vars options])
  ;(remove (publication/Publication{:spreads [{:locked false :id 1}]}) [:spreads])
  ;(remove (account/Organization{:folders ["Example folder"]}) [:folders 4])
)

(defprotocol VOSearchProxy
  ; Search
  ; Search elastic. Returns VO's
  (search [vo] [proxy vo] [proxy vo options])
)

(defprotocol VOHistoryProxy
  ; GetSlice
  ; Get latest slice of history of a VO. Can be limited to a certain amount of :slices.
  (get-slice [proxy vo] [proxy vo options])
)

(deftype ElasticSearchVOProxy [^String index ^org.elasticsearch.client.transport.TransportClient client]
  ;ElasticSearchMapped
  ;(index-name   [this] index)
  ;(mapping-name [this] (str (.. empty-vo voManifest ID)))

  VOProxy
  ; [es es-index ^ValueObject vo options]
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

  (appendTo [this vo path path-vars value]
    (es/appendTo client index vo path (or path-vars ()) value {}))

  (appendTo [this vo path path-vars value options]
    (es/appendTo client index vo path (or path-vars ()) value options))

  (insertAt [this vo path path-vars value]
    (es/insertAt client index vo path (or path-vars ()) value {}))

  (insertAt [this vo path path-vars value options]
    (es/insertAt client index vo path (or path-vars ()) value options))

  (moveTo [this vo path path-vars to]
    (es/moveTo client index vo path (or path-vars ()) to {}))

  (moveTo [this vo path path-vars to options]
    (es/moveTo client index vo path (or path-vars ()) to options))

  (replaceAt [this vo path path-vars value]
    (es/replaceAt client index vo path (or path-vars ()) value {}))

  (replaceAt [this vo path path-vars value options]
    (es/replaceAt client index vo path (or path-vars ()) value options))

  (mergeAt [this vo path path-vars value]
    (es/mergeAt client index vo path (or path-vars ()) value {}))

  (mergeAt [this vo path path-vars value options]
    (es/mergeAt client index vo path (or path-vars ()) value options))

  (removeFrom [this vo path path-vars]
    (es/removeFrom client index vo path (or path-vars ()) {}))

  (removeFrom [this vo path path-vars options]
    (es/removeFrom client index vo path (or path-vars ()) options))

  VOSearchProxy
  ; [es ^ValueObject vo indices & {:as options :keys [ query filter from size types sort highlighting only exclude script-fields preference facets named-filters boost explain version min-score listener ignore-indices routing listener-threaded? search-type operation-threading query-hint scroll source]}]
  (search [this vo]
    (es/search client index vo {}))

  (search [this vo options]
    (es/search client index vo options))

)

(deftype MessagePackVOProxy [^String directory]
  VOProxy
  (get-vo [this vo] 
    (mp/get directory vo))

  (put-vo [this vo]
    (mp/put directory vo {}))

  (put-vo [this vo options]
    (mp/put directory vo options))

  (update [this vo id]
    (mp/update directory vo id {}))

  (update [this vo id options] 
    (mp/update directory vo id options))
  
  (delete [this vo]
    (mp/delete directory vo {}))

  (delete [this vo options]
    (mp/delete directory vo options))
)

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

  (appendTo [this vo path path-vars value]
    (ch/appendTo cluster vo path (or path-vars ()) value {}))

  (appendTo [this vo path path-vars value options]
    (ch/appendTo cluster vo path (or path-vars ()) value options))

  (insertAt [this vo path path-vars value]
    (ch/insertAt cluster vo path (or path-vars ()) value {}))

  (insertAt [this vo path path-vars value options]
    (ch/insertAt cluster vo path (or path-vars ()) value options))

  (moveTo [this vo path path-vars to]
    (ch/moveTo cluster vo path (or path-vars ()) to {}))

  (moveTo [this vo path path-vars to options]
    (ch/moveTo cluster vo path (or path-vars ()) to options))

  (replaceAt [this vo path path-vars value]
    (ch/replaceAt cluster vo path (or path-vars ()) value {}))

  (replaceAt [this vo path path-vars value options]
    (ch/replaceAt cluster vo path (or path-vars ()) value options))

  (mergeAt [this vo path path-vars value]
    (ch/mergeAt cluster vo path (or path-vars ()) value {}))

  (mergeAt [this vo path path-vars value options]
    (ch/mergeAt cluster vo path (or path-vars ()) value options))

  (removeFrom [this vo path path-vars]
    (ch/removeFrom cluster vo path (or path-vars ()) {}))

  (removeFrom [this vo path path-vars options]
    (ch/removeFrom cluster vo path (or path-vars ()) options))

  VOHistoryProxy
  (get-slice [this vo]
    (ch/get-slice cluster vo {}))
  
  (get-slice [this vo options]
    (ch/get-slice cluster vo options))
)

(defn symbolize [sym appendix] (let [sym (if (keyword? sym) (.sym sym) sym)] (symbol (str sym appendix))))

(defn try-proxies [proxies vo fnc options] ; Does not require options. Try is for get only.
  `[~'result (or ~@(map #(list fnc % vo) proxies))])

(defn all-proxies [proxies vo fnc options] 
  (mapcat #(list (symbolize % "-result") (concat (list fnc % vo) options)) proxies))

(def delegator-opts {
  `get-vo 
    [ [['this 'vo]]
      [:pre-get :proxies :post-get]
      try-proxies]
  `put-vo 
    [ [['this 'vo] ['this 'vo 'options]]
      [:pre-put :proxies :post-put]
      all-proxies]
  `update 
    [ [['this 'vo 'id] ['this 'vo 'id 'options]]
      [:pre-update :proxies :post-update]
      all-proxies]
  `delete
    [ [['this 'vo] ['this 'vo 'options]]
      [:pre-delete :proxies :post-delete]
      all-proxies]
  `appendTo
    [ [['this 'vo 'path 'path-vars 'value] ['this 'vo 'path 'path-vars 'value 'options]]
      [:pre-appendTo :proxies :post-appendTo]
      all-proxies]
  `insertAt 
    [ [['this 'vo 'path 'path-vars 'value] ['this 'vo 'path 'path-vars 'value 'options]]
      [:pre-insertAt :proxies :post-insertAt]
      all-proxies]
  `moveTo 
    [ [['this 'vo 'path 'path-vars 'to] ['this 'vo 'path 'path-vars 'to 'options]]
      [:pre-moveTo :proxies :post-moveTo]
      all-proxies]
  `replaceAt
    [ [['this 'vo 'path 'path-vars 'value] ['this 'vo 'path 'path-vars 'value 'options]]
      [:pre-replaceAt :proxies :post-replaceAt]
      all-proxies]
  `mergeAt 
    [ [['this 'vo 'path 'path-vars 'value] ['this 'vo 'path 'path-vars 'value 'options]]
      [:pre-mergeAt :proxies :post-mergeAt]
      all-proxies]
  `removeFrom
    [ [['this 'vo 'path 'path-vars] ['this 'vo 'path 'path-vars 'options]]
      [:pre-removeFrom :proxies :post-removeFrom]
      all-proxies]
  ; 'search 
  ;   [ [['this 'vo] ['this 'vo 'options]]
  ;     [:pre-search :proxies :post-search]
  ;     all-proxies]
  })

(defn filtered [params keep] (prn "Filtered: " params keep))

(defmacro def-vo-proxy-delegator [name & valueobject+options]
  (let [vo+opts (apply hash-map valueobject+options)]
    `(do
       ; (doall (for [[type# opts#] ~vo+opts]
       ;   (prn type# opts#)))
      (def ~name
        (reify VOProxy
          ~@(for [[fnc [params fncs all-or-try]] delegator-opts
                   param params]
              `(~fnc ~param
                (cond
                  ~@(apply concat (for [[type opts] vo+opts]
                    `[(instance? ~type ~'vo)
                      (do (let [
                        ~@(apply concat
                         (when (opts :keep)
                           `[~(into [] (next param)) (filtered ~(into [] (next param)) ~(opts :keep))])
                        ; ~@(apply concat 
                            (for [function fncs]
                              (if (= function :proxies)
                               (all-or-try (opts :proxies) 'vo fnc (drop 2 param))
                               `[~(symbolize function "-result") (filtered ~(opts function))
                                ~@(if-not (nil? (opts function))
                                  ['vo `(or ~(symbolize function "-result") ~'vo)])])))
                        ] 
                        ~(let [
                          res          (if (= fnc `get-vo) ; Exception for a get.
                                        (symbolize :result "")
                                        #_else (if (opts :return-result-of)
                                          (symbolize (opts :return-result-of) "-result")
                                          (symbolize (first (opts :proxies)) "-result")))
                          meta-build  (when (opts :with-meta)
                                        (apply concat 
                                          (for [meta-prox (opts :with-meta)]
                                            (let [result-sym (symbolize meta-prox "-result")
                                                  has-result (or (first (filter #(= result-sym (.sym %)) fncs)) 
                                                                 (first (filter #(= meta-prox %) (opts :proxies)))) ]
                                              (if has-result
                                                [(keyword meta-prox) (symbolize meta-prox "-result")])))))
                          ]
                          (if (and (opts :with-meta) (not= fnc `get-vo))
                            `(with-meta ~res ~meta-build)
                            res))))])))))))
      '~name)))

(def vo-default-opts {
  `get-vo 
    [ [['vo]]
      'get-vo]
  `put-vo 
    [ [['vo] ['vo 'options]]
      'put-vo]
  `update 
    [ [['vo 'id] ['vo 'id 'options]]
      'update]
  `delete
    [ [['vo] ['vo 'options]]
      'delete]
  `appendTo
    [ [['vo 'path 'path-vars 'value] ['vo 'path 'path-vars 'value 'options]]
      'appendTo]
  `insertAt 
    [ [['vo 'path 'path-vars 'value] ['vo 'path 'path-vars 'value 'options]]
      'insertAt]
  `moveTo 
    [ [['vo 'path 'path-vars 'to] ['vo 'path 'path-vars 'to 'options]]
      'moveTo]
  `replaceAt
    [ [['vo 'path 'path-vars 'value] ['vo 'path 'path-vars 'value 'options]]
      'replaceAt]
  `mergeAt 
    [ [['vo 'path 'path-vars 'value] ['vo 'path 'path-vars 'value 'options]]
      'mergeAt]
  `removeFrom
    [ [['vo 'path 'path-vars] ['vo 'path 'path-vars 'options]]
      'removeFrom]
  })

(defn is-published? [vo]
  (= (vo :status) :published))

(defmacro default-vo-proxy [vo & options]
  `(do
    (extend-type ~vo
      VOProxy
        ~@(for [[fnc [params fncname]] vo-default-opts]
          `(~fnc 
            ~@(for [param params]
              `(~param
              ~(do
                (let [option (first (filter #(= fncname (first %)) options))]
                  (if (> (count option) 2)
                      `(cond
                        ~@(apply concat (for [option (apply hash-map (drop 1 option))]
                          `[~(first option) (do ~(concat (list fnc (second option)) param))])))
                    `(do ~(concat (list fnc (second option)) param))))))))))))
