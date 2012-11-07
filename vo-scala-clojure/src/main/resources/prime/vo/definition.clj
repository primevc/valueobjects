;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.definition
  (:use prime.vo.printer, [taoensso.timbre :as timbre :only (trace debug info warn error fatal spy)])
  (:require clojure.pprint)
  (:import (prime.vo ValueObject) (clojure.lang Keyword)))

(set! *warn-on-reflection* true)

(defn companion-object-symbol [votrait]
  (if (class? votrait)
    (symbol (str (.getName ^Class votrait) "$") "MODULE$")
  #_else
    (symbol (.getName (class (.voCompanion ^ValueObject votrait))) "MODULE$")))

(defn field-object-expr [^Class votrait prop]
  (let [field-obj (str (.getName votrait) "$field$")
        prop      (if (= prop 'values) '_values prop)]
    (trace "Searching for field-object" votrait prop "in" field-obj)
    (if (empty? (filter #(= (name prop) (.getName ^java.lang.reflect.Field %)) (.getDeclaredFields ^Class (Class/forName field-obj))))
      `~(symbol (str field-obj prop "$") "MODULE$")
    #_else
      `(.. ~(symbol (str field-obj) "MODULE$") ~prop))))

(defmacro field-object [votrait prop]
  (field-object-expr votrait prop))

(defmacro default-value [votrait prop]
  `(. (field-object ~votrait ~prop) ~'defaultValue))

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

(defn default-value-expr [^Class votrait ^Keyword prop]
  (let [value-expr (macroexpand `(default-value ~votrait ~(.sym prop)))
        value      (eval value-expr)]
    (if (literal-value? value)
      value
    ; literal, return the path to the default value to prevent garbage objects
      value-expr)))

(defmacro to-field-type [votrait field value]
  (let [valueType (eval `(.. ~(field-object-expr votrait field) valueType))]
    (if (or (not (instance? prime.types.package$ValueTypes$Tdef valueType))
            (. ^prime.types.package$ValueTypes$Tdef valueType ref))
      `(.. ~(macroexpand `(field-object ~votrait ~(symbol (name field)))) ~'valueType (~'convert ~value))
    ;else Tdef: not a ref
      (let [valueType ^prime.types.package$ValueTypes$Tdef valueType]
        `(~(.. valueType keyword sym) ~value)))))

(defn vo-constructor-arglist-from-map [^Class votrait props]
  (map #(do
    `(let [~'v (~'value-map ~%)]
      (if  ~'v `(to-field-type ~~votrait ~(.sym ^Keyword ~%) ~~'v)
        #_else '~(default-value-expr votrait %)
      ))) props))

(defn eval? [expr]
  "Returns the result of `(eval expr)` or false if any Exception is thrown."
  (try (eval expr)
    (catch Exception e#
      (trace e# "Tried to eval:" (let [w (java.io.StringWriter.)] (clojure.pprint/pprint expr w) (.toString w)))
      false)))


;
; VO definition macro
;

(defn intern-vo-expr "Creates a var using (intern ..) and returns the symbol of it" [^ValueObject vo construct-expr]
  (if (empty? vo)
    (list '.empty (companion-object-symbol vo))
  #_else
    (let [obj-ns-sym  (.getName ^clojure.lang.Namespace (create-ns (symbol (str (.. vo getClass getPackage getName) ".-interned"))))
          intern-name (str (.. vo getClass getSimpleName) (.hashCode vo))
          ^clojure.lang.Var
          interned    (or
            (resolve (symbol (.getName obj-ns-sym) intern-name))
            (do (debug "interning:" #_[construct-expr " => "] vo "in:" (str obj-ns-sym "/" intern-name))
              (intern obj-ns-sym (symbol intern-name) vo)))]
      (symbol (str (.ns interned)) (str (.sym interned))))))

(defn defvo-body
  [^Class votrait runtime-constructor props]
  (assert (class? votrait))
  (assert (symbol? runtime-constructor))
  (assert (sequential? props))
  (assert (every? keyword? props))
  (let [args (vo-constructor-arglist-from-map votrait props)]
    `(if (map? ~'value-map)
      (let [~'construct-expr# (list '.apply '~(companion-object-symbol votrait) ~@args)
            ~'instance# (eval? ~'construct-expr#)]
        (if ~'instance#
          (intern-vo-expr ~'instance# ~'construct-expr#)
          ~'construct-expr#))
    ;else: Runtime argument; return fn instead
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
      (eval '(defmacro ~macroname
        ([] ~converterfn-name)
        ([~'value-map-expr]
          (let [~'value-map (or
                              (if-let [~'value-map (eval? ~'value-map-expr)] (if (map? ~'value-map) ~'value-map ~'value-map-expr))
                              ~'value-map-expr)]
            ~macro-body))))
  )))
