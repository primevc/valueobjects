;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns clojure.lang)

(def ^{:private true} reduce1              #'clojure.core/reduce1)
(def ^{:private true} emit-method-builder  #'clojure.core/emit-method-builder)
(def ^{:private true} assert-same-protocol #'clojure.core/assert-same-protocol)

(defn- emit-protocol-without-interface [name opts+sigs]
  (let [iname (symbol (str (munge (namespace-munge *ns*)) "." (munge name)))
        [opts sigs]
        (loop [opts {:on (list 'quote iname) :on-interface iname} sigs opts+sigs]
          (condp #(%1 %2) (first sigs)
            string? (recur (assoc opts :doc (first sigs)) (next sigs))
            keyword? (recur (assoc opts (first sigs) (second sigs)) (nnext sigs))
            [opts sigs]))
        sigs (reduce1 (fn [m s]
                       (let [name-meta (meta (first s))
                             mname (with-meta (first s) nil)
                             [arglists doc]
                               (loop [as [] rs (rest s)]
                                 (if (vector? (first rs))
                                   (recur (conj as (first rs)) (next rs))
                                   [(seq as) (first rs)]))]
                         (when (some #{0} (map count arglists))
                           (throw (IllegalArgumentException. (str "Protocol fn: " mname " must take at least one arg"))))
                         (assoc m (keyword mname)
                                (merge name-meta
                                       {:name (vary-meta mname assoc :doc doc :arglists arglists)
                                        :arglists arglists
                                        :doc doc}))))
                     {} sigs)]
  `(do
     (defonce ~name {})
     (alter-meta! (var ~name) assoc :doc ~(:doc opts))
     (#'assert-same-protocol (var ~name) '~(map :name (vals sigs)))
     (alter-var-root (var ~name) merge
                     (assoc ~opts
                       :sigs '~sigs
                       :var (var ~name)
                       :method-map
                         ~(and (:on opts)
                               (apply hash-map
                                      (mapcat
                                       (fn [s]
                                         [(keyword (:name s)) (keyword (or (:on s) (:name s)))])
                                       (vals sigs))))
                       :method-builders
                        ~(apply hash-map
                                (mapcat
                                 (fn [s]
                                   [`(intern *ns* (with-meta '~(:name s) (merge '~s {:protocol (var ~name)})))
                                    (emit-method-builder (:on-interface opts) (:name s) (:on s) (:arglists s))])
                                 (vals sigs)))))
     (-reset-methods ~name)
     '~name)))

; Instead of copying the emit-protocol implementation and remove the call to gen-interface, 
;  I (Danny) tried to stub out gen-interface like so:

;#_(defmacro -stub [& a] (println "stubbed" a))
;(alter-meta! #'-stub dissoc :macro true)
;
;#_(defmacro def-existing-protocol [name & opts+sigs]
;  (println "Defining protocol with existing interface: " (Class/forName (str *ns* "." name)))
;  (with-redefs [gen-interface #'-stub]
;    (alter-meta! #'gen-interface assoc :macro true)
;    (eval `(defprotocol ~name ~@opts+sigs)))
;  (assert (not= #'gen-interface #'-stub)))


;
; I also tried this variation:

;#_(defmacro def-existing-protocol [name & opts+sigs]
;  (with-bindings {#'*compile-files* false}
;    (let [clz (Class/forName (str *ns* "." name))]
;      (println "Defining protocol with existing interface:" clz)
;      (. ^java.lang.ClassLoader (deref clojure.lang.Compiler/LOADER) defineClass (.getName clz) (org.apache.commons.io.IOUtils/toByteArray (.getResourceAsStream (.getClassLoader clz) (str (clojure.string/replace (.getName clz) "." "/") ".class"))) {})
;      (#'clojure.core/emit-protocol name opts+sigs))))

;
; But after wasting almost a full day on this, gave up :-/

(defmacro def-existing-protocol
  "Exactly like defprotocol, but does not generate a Java interface."
  [name & opts+sigs]
  (emit-protocol-without-interface name opts+sigs))


(def-existing-protocol IFn
  (applyTo [ifn ^clojure.lang.ISeq arglist])
  (invoke  [ifn]
           [ifn a]
           [ifn a b]
           [ifn a b c]
           [ifn a b c d]
           [ifn a b c d e]
           [ifn a b c d e f]
           [ifn a b c d e f g]
           [ifn a b c d e f g h]
           [ifn a b c d e f g h i]
           [ifn a b c d e f g h i j]
           [ifn a b c d e f g h i j k]
           [ifn a b c d e f g h i j k l]
           [ifn a b c d e f g h i j k l m]
           [ifn a b c d e f g h i j k l m n]
           [ifn a b c d e f g h i j k l m n o]
           [ifn a b c d e f g h i j k l m n o p]
           [ifn a b c d e f g h i j k l m n o p q]
           [ifn a b c d e f g h i j k l m n o p q & r]
))


;
; Scala implementation
;
(extend-type scala.runtime.AbstractFunction1
  IFn
    (invoke [f a] (.apply f a)))


#_(defn invoke [fnobj arg]
  (if (isa? (class fnobj) clojure.lang.IFn)
    (fnobj arg)
  #_else
    (.apply ^scala.runtime.AbstractFunction1 fnobj arg)))
