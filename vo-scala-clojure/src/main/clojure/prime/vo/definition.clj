;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.definition
  (:use prime.vo.printer, [taoensso.timbre :as timbre :only (trace debug info warn error fatal spy)])
  (:require clojure.pprint, prime.types, [clojure.set :as set])
  (:import (prime.vo ValueObject) (clojure.lang Symbol Keyword) prime.types.Conversion$NoInputException$))

(set! *warn-on-reflection* true)

(defn eval?
 [expr]
  "Returns the result of `(eval expr)` or :clojure.lang/eval-failed if any Exception is thrown."
  (try (do (trace "About to eval:" (prn-str expr)) (eval expr))
    (catch Exception e#
      (trace e# "Tried to eval:" (let [w (java.io.StringWriter.)] (clojure.pprint/pprint expr w) (.toString w)))
      :clojure.lang/eval-failed)))

;
;
;

(defn companion-object-symbol [votrait]
  (if (class? votrait)
    (symbol (str (.getName ^Class votrait) "$") "MODULE$")
  #_else
    (symbol (.getName (class (.voCompanion ^ValueObject votrait))) "MODULE$")))

(defn field-object-expr [^Class votrait ^Keyword prop]
  (let [field-obj (str (.getName votrait) "$field$")
        prop      (if (= prop :values) '_values (.sym prop))]
    (trace "Searching for field-object" votrait prop "in" field-obj)
    (if (not-any? #(= (name prop) (.getName ^java.lang.reflect.Field %)) (.getDeclaredFields ^Class (Class/forName field-obj)))
      `~(symbol (str field-obj prop "$") "MODULE$")
    #_else
      `(.. ~(symbol (str field-obj) "MODULE$") ~prop))))

(defmacro field-object [votrait prop]
  (field-object-expr (eval votrait) prop))

(defmacro default-value [votrait prop]
  `(. ~(field-object-expr votrait prop) ~'defaultValue))

(defn vo-converterfn-expr [^Class votrait fn-name]
  `(defn ~fn-name 
    ([value#] (.valueOf ~(companion-object-symbol votrait) value#))))

(defn literal-value?
  "Returns true  for literal values like booleans, strings and numbers.
           false for ValueObjects and vectors.
  Literals:
    Strings - Enclosed in \"double quotes\". May span multiple lines. Standard Java escape characters are supported.
    Numbers - as per Java, plus indefinitely long integers are supported, as well as ratios, e.g. 22/7. Floating point numbers with an M suffix are read as BigDecimals. Integers can be specified in any base supported by Integer.parseInt(), that is any radix from 2 to 36; for example 2r101010, 8r52, 36r16, and 42 are all the same Integer.
    Characters - preceded by a backslash: \\c. \\newline, \\space and \\tab yield the corresponding characters.
    nil Means 'nothing/no-value'- represents Java null and tests logical false
    Booleans - true and false
    Keywords"
  [value]
  (or (nil?     value)
      (string?  value)
      (number?  value)
      (char?    value)
      (true?    value)
      (false?   value)
      (symbol?  value)
      (keyword? value)
  ))

(defn stable-argument?
  "A Stable argument means it is either a literal-value (including symbols),
    or a pure function with only pure/literal arguments
    or a collection with only pure/stable content"
  [value]
  (try (or
    (literal-value?  value)
    (if (or (vector? value) (set? value)) (every? stable-argument? value))
    (if (map?        value) (every? stable-argument? (vals value)))
    (if (list?       value) (and (:pure (meta (ns-resolve *ns* (first value))))
                                 (every? stable-argument?      (rest  value))))

    (do (trace "Unstable: " value) false))
  (catch Exception e (do (trace e "Unstable: " value) false))))

(defn unstable-argument? [value]
  (not (stable-argument?  value)))

(defn default-value-expr [^Class votrait ^Keyword prop]
  (let [value-expr (macroexpand `(default-value ~votrait ~prop))
        value      (eval value-expr)]
    (if (literal-value? value)
      value
    ; literal, return the path to the default value to prevent garbage objects
      value-expr)))

(defmacro to-field-type [votrait prop value-expr]
  (let [^Class votrait (if (class? votrait) votrait (eval votrait))
        valueType-expr `(. ~(field-object-expr votrait prop) ~'valueType)
             valueType (eval valueType-expr)]
    (if (or (not (instance? prime.types.package$ValueTypes$Tdef valueType))
            (. ^prime.types.package$ValueTypes$Tdef valueType ref))
      `(try (. ~valueType-expr (~'convert ~value-expr))
        (catch Conversion$NoInputException$ e# ~(default-value-expr votrait prop)))
    ;else Tdef: not a ref
      (let [valueType ^prime.types.package$ValueTypes$Tdef valueType]
        `(~(.. valueType keyword sym) ~value-expr)))))

(defn vo-constructor-arglist-from-map [^Class votrait props]
  (map #(do
    `(let [~'v (~'value-map ~% :prime.vo/default)
           ~'x (if (not= :prime.vo/default ~'v) `(to-field-type ~~votrait ~~% ~~'v)
                #_else '~(default-value-expr votrait %))
           ~'e (if (and (not= :prime.vo/default ~'v) (stable-argument? ~'v)) (eval? ~'x) #_else :clojure.lang/eval-failed)]
      (if (and (not= :clojure.lang/eval-failed ~'e)
               (or (literal-value? ~'e) (instance? prime.types.EnumValue ~'e)))
        ~'e #_else ~'x)))
    props))



;
; VO definition macro
;

(defn intern-vo-expr "Creates a var using (intern ..) and returns the symbol of it" [^ValueObject vo construct-expr]
  (if (and (empty? vo) (not (nil? vo)))
    (list '.empty (companion-object-symbol vo))
  #_else
    construct-expr))

; Attempt at initializing at runtime, almost defeating the purpose. Generated code is buggy somehow.
#_(defn intern-vo-expr "Creates a var using (intern ..) and returns the symbol of it" [^ValueObject vo construct-expr]
  (trace (prn-str "Possibly interning: " vo ":" construct-expr))
  (if (empty? vo)
    (list '.empty (companion-object-symbol vo))
  #_else
    (let [intern-name (str (.. vo getClass getSimpleName) (.hashCode vo))
          obj-ns-sym  (symbol (str (.. vo getClass getPackage getName) ".-interned"))]
      `(let [obj-ns# (.getName ^clojure.lang.Namespace (create-ns '~obj-ns-sym))
             vo-sym# (resolve '~(symbol (name obj-ns-sym) intern-name))]
        (debug ~(str "interning: " #_[construct-expr " => "] vo " in: " (name obj-ns-sym) "/" intern-name))
        (if (and vo-sym# (bound? vo-sym#))
          vo-sym#
          (do
            (intern obj-ns# '~(symbol intern-name) ~construct-expr)
            vo-sym#))))))

; Initialization at macro time: doesn't work with AOT compilation
#_(defn intern-vo-expr "Creates a var using (intern ..) and returns the symbol of it" [^ValueObject vo construct-expr]
  (trace (prn-str "Possibly interning: " vo ":" construct-expr))
  (if (empty? vo)
    (list '.empty (companion-object-symbol vo))
  #_else
    (let [obj-ns-sym  (.getName ^clojure.lang.Namespace (create-ns (symbol (str (.. vo getClass getPackage getName) ".-interned"))))
          intern-name# (str (.. vo getClass getSimpleName) (.hashCode vo))
         ]
      (or
        (if-let [vo-sym# (resolve (symbol (.getName obj-ns-sym) intern-name))] (if (bound? vo-sym#) vo-sym#))
        (do (debug (str "interning: " #_[construct-expr " => "] vo " in: " obj-ns-sym "/" intern-name))
          (binding [*ns* (the-ns obj-ns-sym)] (eval `(def ~(symbol intern-name) ~construct-expr)))))
      )))

(defn defvo-body
  "The argument to the constructor macro has to be stable before interning is considered.
   This fixes too aggressive interning, for example objects defined with (ObjectId.)
   Purity/stability is checked via: (stable-argument? value-map)"
  [^Class votrait runtime-constructor props]
  (assert (class? votrait))
  (assert (symbol? runtime-constructor))
  (assert (or (empty? props) (sequential? props)))
  (assert (every? keyword? props))
  (trace "defvo-body")
  (let [args (vo-constructor-arglist-from-map votrait props)]
    (trace "vo-constructor-arglist-from-map:" (print-str args))
    `(cond
      (instance? ~votrait ~'value-map)
        (intern-vo-expr ~'value-map (list ~runtime-constructor ~'value-map))

      (map? ~'value-map)
        (let [~'construct-expr# (list '.apply '~(companion-object-symbol votrait) ~@args)
              ~'instance#       (if ~'stable-value-map (eval? ~'construct-expr#))
              ~'undefined       (set/difference (set (map key ~'value-map)) ~(set props))]
          (assert (empty? ~'undefined), (str "\n  " ~votrait " defined fields,\n  are:   " (prn-str ~@props) ",\n  not! " ~'undefined))
          (if (not= :clojure.lang/eval-failed ~'instance#)
            (intern-vo-expr ~'instance# ~'construct-expr#)
          #_else
            ~'construct-expr#))

      :else ;Runtime argument; return fn instead
        `(~~runtime-constructor ~~'value-map);)
    )))

(defmacro defvo
  "Defines a VO-literal macro and converter fn inside the namespace of the given trait's package.

  Inputs 
    votrait:  the VO Scala trait/interface
    props:    the constructor keywords in order of the Scala object companion apply() method.
  
  Example:
    (defvo vo.Point :x :y)  ; Creates the macro Point and conversion fn to-Point.
    (Point{:x 1 :y 2})      ; Instantiates a PointVO."
    [votrait-expr & props]
  (let [^Class votrait      (let [r (eval? votrait-expr)] (assert (class? r) "votrait must be a VO trait/interface") r)
        empty-vo-instance   (eval (list '.empty (companion-object-symbol votrait)))
        macroname           (symbol (.getSimpleName votrait))
        converterfn-name    (symbol (str "to-" (.getSimpleName votrait)))
        runtime-constructor (vo-converterfn-expr votrait converterfn-name)
        macro-body          (defvo-body votrait converterfn-name props)]
    (assert (not (nil? empty-vo-instance)))
    `(binding [*ns* (create-ns '~(symbol (.. votrait getPackage getName)))]
      (eval '~runtime-constructor)
      (eval '(do
        (defmacro ^:pure ~macroname
          ([] ~converterfn-name)
          ([~'value-map-expr]
          (let [~'stable-value-map (stable-argument? ~'value-map-expr)
                ~'value-map        (if (and ~'stable-value-map (list? ~'value-map-expr)) (eval ~'value-map-expr) ~'value-map-expr)]
            ~macro-body)))
        (alter-meta! (var ~macroname) assoc :pure true)
      ))
)))
