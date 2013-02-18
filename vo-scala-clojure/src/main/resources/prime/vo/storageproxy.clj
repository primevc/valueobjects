;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.storageproxy
	(:require [prime.vo.util.elasticsearch :as es]))

(defprotocol VOProxy
  "Doc string"
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

  ; Update-if
  ; Same as update but only executes when certain requirements are met.
  #_(update-if [proxy predicate options])

  ; [es vo path value pos]
  (insertAt [vo] [proxy vo])

  ; [es vo path pos]
  (moveTo [vo] [proxy vo])

  ; [es ^ValueObject vo id & {:as options :keys [index]}]
  (appendTo [vo] [proxy vo] [proxy vo id] [proxy vo id options])

  ; [es vo pos value]
  (replaceAt [vo] [proxy vo])

  ; Delete
  ; Deletes a VO. Use with care :-)
  (delete [vo] [proxy vo] [proxy vo options])
)

(defprotocol VOSearchProxy
  ; Search
  ; Search elastic. Returns VO's
  (search [vo] [proxy vo] [proxy vo options])
)

(deftype ElasticSearchVOProxy [^String index ^org.elasticsearch.client.transport.TransportClient client]
  ;ElasticSearchMapped
  ;(index-name   [this] index)
  ;(mapping-name [this] (str (.. empty-vo voManifest ID)))

  VOProxy
  ; [es es-index ^ValueObject vo options]
  (get-vo [this vo] 
  	(es/get client index vo))

  ; [es ^ValueObject vo & {:as options :keys [index]}]
  (put-vo [this vo]
    (es/put client vo index {}))

  (put-vo [this vo options]
    (es/put client vo index options))

  ; [es ^ValueObject vo id & {:as options :keys [index]}]
  (update [this vo id]
    (es/update client vo id index {}))

  (update [this vo id options] 
  	(es/update client vo id index options))
  
  ; [es vo path value pos]
  (insertAt [vo] vo)

  ; [es vo path pos]
  (moveTo [vo] vo)

  ; [es ^ValueObject vo id & {:as options :keys [index]}]
  (appendTo [this vo id]
    (es/appendTo client vo id index {}))

  (appendTo [this vo id options]
  	(es/appendTo client vo id index options))

  ; [es vo pos value]
  (replaceAt [vo] vo)

  ; [es ^ValueObject vo & {:as options :keys [index]}]
  (delete [this vo]
    (es/delete client vo index {}))

  (delete [this vo options]
    (es/delete client vo index options))

  VOSearchProxy
  ; [es ^ValueObject vo indices & {:as options :keys [ query filter from size types sort highlighting only exclude script-fields preference facets named-filters boost explain version min-score listener ignore-indices routing listener-threaded? search-type operation-threading query-hint scroll source]}]
  (search [this vo]
    (es/search client index vo {}))

  (search [this vo options]
    (es/search client index vo options))
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
  `appendTo
    [ [['this 'vo 'id] ['this 'vo 'id 'options]]
      [:pre-appendTo :proxies :post-appendTo]
      all-proxies]
  `delete
    [ [['this 'vo] ['this 'vo 'options]]
      [:pre-delete :proxies :post-delete]
      all-proxies]  
  ; 'search 
  ;   [ [['this 'vo] ['this 'vo 'options]]
  ;     [:pre-search :proxies :post-search]
  ;     all-proxies]
  })

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
                      ~@(apply concat (for [function fncs]
                          (if (= function :proxies)
                             (all-or-try (opts :proxies) 'vo fnc (drop 2 param))
                             [(symbolize function "-result") (opts function)]
                              )))
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
  `appendTo
    [ [['vo 'id] ['vo 'id 'options]]
      'appendTo]
  `delete
    [ [['vo] ['vo 'options]]
      'delete]
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


