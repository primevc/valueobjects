;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.proxy
  "This namespace defines the VOProxy, VOSearchProxy and VOHistoryProxy
  protocols. The namespace also includes helper functions for easy
  creation of implementations of these protocols.")


;;; Protocol definitions.

(defprotocol VOProxy
  "WARNING: Proxy cannot guarantee consistency!"

  ;; Standard storage functions.
  (get-vo [vo] [proxy vo]
    "Get will get you a VO based on ID")

  (put-vo [vo] [proxy vo] [proxy vo options]
    "Putting will replace the existing VO with the new one. Requires ID")

  (update [vo] [proxy vo] [proxy vo id] [proxy vo id options]
    "Updating will add the new VO or replace the data if it already exists.")

  (delete [vo] [proxy vo] [proxy vo options]
    "Deletes a VO. Use with care.")

  ;; Delta changes functions.
  (append-to [vo path path-vars value] [this vo path path-vars value] [this vo path path-vars value options])

  (insert-at [vo path path-vars value] [proxy vo path path-vars value] [proxy vo path path-vars value options])

  (move-to [vo path path-vars to] [proxy vo path path-vars to] [proxy vo path path-vars to options])

  (replace-at [vo path path-vars value] [proxy vo path path-vars value] [proxy vo path path-vars value options])

  (merge-at [vo path path-vars value] [prxoy vo path path-vars value] [proxy vo path path-vars value options])

  (remove-from [vo path path-vars] [proxy vo path path-vars] [proxy vo path path-vars options]))


(defprotocol VOSearchProxy
  "Search for VOs."
  (search [vo] [proxy vo] [proxy vo options]
    "Search for VOs."))


(defprotocol VOHistoryProxy
  "Get the history of a particular VO."
  (get-slice [proxy vo] [proxy vo options]
    "Get latest slice of history of a VO. Can be limited to a certain amount of :slices."))


;;; Helper functions.

(defn symbolize [sym appendix] (symbol (str (name sym) appendix)))


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
  `append-to
    [ [['this 'vo 'path 'path-vars 'value] ['this 'vo 'path 'path-vars 'value 'options]]
      [:pre-append-to :proxies :post-append-to]
      all-proxies]
  `insert-at
    [ [['this 'vo 'path 'path-vars 'value] ['this 'vo 'path 'path-vars 'value 'options]]
      [:pre-insert-at :proxies :post-insert-at]
      all-proxies]
  `move-to
    [ [['this 'vo 'path 'path-vars 'to] ['this 'vo 'path 'path-vars 'to 'options]]
      [:pre-move-to :proxies :post-move-to]
      all-proxies]
  `replace-at
    [ [['this 'vo 'path 'path-vars 'value] ['this 'vo 'path 'path-vars 'value 'options]]
      [:pre-replace-at :proxies :post-replace-at]
      all-proxies]
  `merge-at
    [ [['this 'vo 'path 'path-vars 'value] ['this 'vo 'path 'path-vars 'value 'options]]
      [:pre-merge-at :proxies :post-merge-at]
      all-proxies]
  `remove-from
    [ [['this 'vo 'path 'path-vars] ['this 'vo 'path 'path-vars 'options]]
      [:pre-remove-from :proxies :post-remove-from]
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
                             `[~(symbolize function "-result") ~(opts function)
                              ~@(if-not (nil? (opts function))
                                ['vo
                                `(or ~(symbolize function "-result") ~'vo)])])))
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
  `append-to
    [ [['vo 'path 'path-vars 'value] ['vo 'path 'path-vars 'value 'options]]
      'append-to]
  `insert-at
    [ [['vo 'path 'path-vars 'value] ['vo 'path 'path-vars 'value 'options]]
      'insert-at]
  `move-to
    [ [['vo 'path 'path-vars 'to] ['vo 'path 'path-vars 'to 'options]]
      'move-to]
  `replace-at
    [ [['vo 'path 'path-vars 'value] ['vo 'path 'path-vars 'value 'options]]
      'replace-at]
  `merge-at
    [ [['vo 'path 'path-vars 'value] ['vo 'path 'path-vars 'value 'options]]
      'merge-at]
  `remove-from
    [ [['vo 'path 'path-vars] ['vo 'path 'path-vars 'options]]
      'remove-from]
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
