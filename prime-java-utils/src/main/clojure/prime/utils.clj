;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.utils
  (:import [java.io PipedInputStream PipedOutputStream]
           [org.apache.commons.io.input TeeInputStream]))


(defmacro or-not
  "Evaluates exprs one at a time, from left to right. If a form
  returns a falsey value for the supplied predicate, it returns that value,
  otherwise it returns the value of the last expression.

  Example:
  (or-not nil? nil :a :b) => :a
  (or-not nil? nil)       => nil
  (or-not map? {})        => {}"
  ([pred x] x)
  ([pred x & next]
      `(let [or# ~x]
         (if-not (~pred or#) or# (or-not ~pred ~@next)))))


(defmacro mapify
  "Given some symbols, construct a map with the symbols as keys, and the value
  of the symbols as the map values. Unbound symbols or those with nil values are
  not included in the resulting map. An example:

  (let [foo \"bar\"]
    (mapify foo))    ; => {:foo \"bar\"}"
  [& symbols]
  `(into {} (filter second ~(vec (for [item symbols] [(keyword item) item])))))


(defmacro try-let
  "Behaves like try except the symbols in bindings are available to the catch
  and finally clauses. The bindings' values are evaluated sequentially. If an
  exception is thrown while evaluating a binding value, it and every subsequent
  binding value will be nil.  The catch clauses handle exceptions thrown by the
  body of the try and by the evaluation of binding values.

  Example:
  (try-let [from (API/open from-addr)
            to   (API/open to-addr)]
    (do-stuff from to)
    (finally
      (if to (doto to .flush .close))
      (if from (.close from))))"
  {:arglists '([[bindings*] try-expr* catch-clause* finally-clause?])}
  [bindings & exprs]
  (let [ts (gensym)
        names# (take-nth 2 bindings)
        valex# (take-nth 2 (drop 1 bindings))
        [body# cf#] (split-with (comp not #{'catch 'finally} first) exprs)]
    `(let [~ts nil
           ~@(interleave names# (repeat nil))
           ~@(interleave
               (map vector names# (repeat ts))
               (for [v valex#]
                 `(if ~ts [nil ~ts]
                   (try [~v nil]
                     (catch Throwable t# [nil t#])))))]
      (try
        (if ~ts
          (throw ~ts)
          (do ~@body#))
        ~@cf#))))


(defmacro with-resource
  "Use a resource, of which one can supply the clean-up function
  oneself. For example:

  (with-resource [c (start-cassandra)] #(stop-cassandra %)
    (do-stuff-with c))"
  [binding clean-fn & body]
  (let [bindsym (gensym "binding")]
    `(let [~bindsym ~(second binding)
           ~(first binding) ~bindsym]
       (try
         ~@body
         (finally
           (~clean-fn ~bindsym))))))


(defmacro guard-let
  "Like the if-let, except one supplies the predicate oneself. The binding
  takes the form of [symbol value-expression guard-type predicate], where
  guard-type can be :when or :when-not. For example:

  (guard-let [foo (some-string-function) :when-not clojure.string/blank?]
    (do-stuff-with foo)
    (do-else))"
  [[sym val grd-type grd] when else]
  (let [grd-fn (condp = grd-type
                 :when `(~grd ~sym)
                 :when-not `(not (~grd ~sym))
                 (throw (IllegalArgumentException. (str "unsupported guard type: " grd-type))))]
    `(let [~sym ~val]
       (if ~grd-fn ~when ~else))))


(defmacro memoize-form
  "Memoize-form is like memoize, but takes a form instead of a function, and
  evaluates to a value, not a function per se. By having a form, one can
  preprocess arguments, both around memoize-form as well as inside the form. For
  example:

  (defn format-name* [s]
    (println \"Not cached!\")
    (clojure.string/capitalize s)) ; pretend this is expensive.

  (defn format-name [s]
    (let [lowercase (clojure.string/lower-case s)]
      (memoize-form (format-name* (.trim lowercase)))))

  (format-name \"nemo\")
  Not cached
  ;=> \"Nemo\"

  (format-name \"   NEMO\")
  ;=> \"Nemo\"

  Note that the form should be calling a named function, not a macro. Anonymous
  functions are not supported yet."
  [form]
  (let [sym (gensym (str "memoized-"
                         (clojure.string/replace (str (first form)) #"[/\.]" "-")
                         "-"))]
    (eval `(def ~(with-meta sym (assoc (meta sym) :private true)) ; ^:private does not seem to work
             (memoize ~(first form))))
    `(~sym ~@(rest form))))


(defmacro if-let*
  "Have multiple bindings, which must all be truthy for the then form
  to evaluate. As soon as one binding is falsey, the else form is
  evaluated. The if-let* does not support leaving out the else form,
  where the core if and if-let do. This is because I believe using
  when/when-let/when-let* over if/if-let/if-let* without an else
  clause increases readability."
  [[var expr & bindings] then else]
  (when-not (even? (count bindings))
    (throw (IllegalArgumentException. "must have even numbers of forms in binding vector.")))
  `(if-let [~var ~expr]
     ~(if (seq bindings)
        `(if-let* ~bindings ~then ~else)
        `~then)
     ~else))


(defmacro when-let*
  "Have multiple bindings, which must all be truthy for the body to
  evaluate. As soon as one binding is falsey, nil is returned."
  [bindings & body]
  `(if-let* ~bindings (do ~@body) nil))


(defn index-of
  "Returns the index of a value in a sequence, or nil if it is not
  found."
  [value sequence]
  (first (keep-indexed (fn [index item] (when (= value item) index)) sequence)))


(defmacro forcat
  "For's analog of mapcat. Equivalent to (apply concat (for [...] ...))."
  [seq-exprs body-expr]
  `(apply concat (for ~seq-exprs ~body-expr)))


(defmacro implement-by-forwarding
  "Forwards all methods in `protocol` to calls to `impl-instance`.
  Drops the first `this` argument in the forwarded call:

    user=> (defprotocol B (method [x y z]))
    user=> (macroexpand-1 '(implement-by-forwarding A B implementation))
    (clojure.core/extend-type A
      B
      (method ([y z] (user/method implementation y z))))

  Useful for quicker VOProxy forwarding than `prime.vo.proxy/default-vo-proxy`.
  You have no choice of forwarded implementation though. This
  macro uses the meta-data already supplied by the protocol,
  whereas `default-vo-proxy` keeps a separate map of meta-data."
  [type protocol impl-instance]
  (let [ns-str (str (:ns (meta (eval `(var ~protocol)))))
        forwards (for [sig (:sigs (eval protocol))
                       :let [{:keys [name arglists]} (val sig)
                             qname (symbol ns-str (str name))]]
                   `(~name ~@(for [arglist arglists
                                   :let [arglist (vec (rest arglist))]
                                   :when (not (empty? arglist))]
                               `(~arglist (~qname ~impl-instance ~@arglist)))))]
    `(extend-type ~type
       ~protocol
       ~@forwards)))


(defn eavesdrop-inputstream
  "Given an InputStream, this function returns a pair of InputStreams.
  The first one reads directly from the given InputStream, whereas the
  second will have those read bytes as its data. Be sure to read the
  returned InputStreams in separate threads, to avoid deadlocks.
  Optionally, a buffer size can be specified, otherwise the default of
  PipedInputStream is used. Note that when this buffer is full,
  reading from the first stream blocks. Thus, make sure the second
  stream is being read as well."
  [in & [buffer-size]]
  (let [pis (if buffer-size (PipedInputStream. (int buffer-size)) #_else (PipedInputStream.))
        pos (PipedOutputStream. pis)]
    [(TeeInputStream. in pos true) pis]))


(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level.

  (deepmerge + {:a {:b {:c 1 :d {:x 1 :y 2}} :e 3} :f 4}
               {:a {:b {:c 2 :d {:z 9} :z 3} :e 100}})
  ;=> {:a {:b {:z 3, :c 3, :d {:z 9, :x 1, :y 2}}, :e 103}, :f 4}

  From clojure.contrib.map-utils."
  [f & maps]
  (apply
   (fn m [& maps]
     (if (every? map? maps)
       (apply merge-with m maps)
       (apply f maps)))
   maps))


(defmacro with-debug
  "A debug macro, which keeps the `pre` forms iff the
  *compiler-options* map contains a set under the keyword :with-debug
  holding the given `name` symbol. Any pre form that is not a list,
  will be wrapped with a `prn` call. The body has an implicet do. For
  example:

  (binding [*compiler-options* '{:with-debug #{voproxy-core}}]
    (eval '(with-debug voproxy-core
             [(println \"INPUT:\")
              input]
             (work-on input))))"
  [name pre & body]
  (if (get-in *compiler-options* [:with-debug name])
    (let [pre (map (fn [form] (if-not (list? form) `(prn ~form) form)) pre)]
      `(do ~@pre ~@body))
    `(do ~@body)))


;; Locking by value functions.

(defn mk-lock-pool
  "Create a new lock pool for use with `with-lock-by-value`. Using a
  pool ensures that locks are acquired within the same context, and
  don't interfere with other contexts."
  [] (atom {}))

(defn with-lock-by-value
  "Execute a function `f` by synchronizing on a value `val` (does not
  necessarily need to be the same object), applying `args` to the
  function. The original lock object is released when it is not used
  anymore.

  Supply a lock `pool`, acquired by `mk-lock-pool` as the context in
  which to acquire and reuse the lock values."
  [pool val f & args]
  (let [locks (swap! pool update-in [val]
                     (fn [[lock-val counter]]
                       [(or lock-val val)
                        (or (and counter (inc counter)) 1)]))]
    (try
      (locking (get-in locks [val 0])
        (apply f args))
      (finally
        (swap! pool (fn [locks]
                      (if (= (get-in locks [val 1]) 1)
                        (dissoc locks val)
                        (update-in locks [val 1] dec))))))))
