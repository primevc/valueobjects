;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.definition
  (:use prime.vo.printer, trammel.core)
  (:import (prime.vo ValueObject)))


(defn companion-object-symbol [^Class votrait]
  (symbol (str (.getName votrait) "$") "MODULE$"))

(defmacro default-value [votrait prop]
  (let [field-obj (symbol (str (.getName votrait) "$field$"))]
    (if (empty? (filter #(= (name prop) (.getName %)) (.getDeclaredFields (eval field-obj))))
      `(.. ~(symbol (str field-obj prop "$") "MODULE$") ~'defaultValue)
    #_else
      `(.. ~(symbol (str field-obj) "MODULE$") ~prop ~'defaultValue))))

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

(defn default-value-expr [^Class votrait ^clojure.lang.Keyword prop]
  (let [value-expr (macroexpand `(default-value ~votrait ~(.sym prop)))
        value      (eval value-expr)]
    (if (literal-value? value)
      value
    ; literal, return the path to the default value to prevent garbage objects
      value-expr)))

(defn vo-constructor-arglist-from-map [^Class votrait props]
  (map #(macroexpand `(let [~'v (~% ~'value-map)] (if ~'v ~'v '~(default-value-expr votrait %)))) props))

(defn eval? [expr]
  "Returns the result of `(eval expr)` or false if any Exception is thrown."
  (try (eval expr)
    (catch Exception e#
      ;(do (println "evals=false" e#)
          false)));)

(defmacro if-evals 
  ([expr then]      `(when (eval? ~expr) ~then))
  ([expr then else] `(if   (eval? ~expr) ~then ~else)))


;
; VO definition macro
;

(defconstrainedfn defvo-body
  [^Class votrait runtime-constructor props] [(class? votrait) (symbol? runtime-constructor)
                                              (sequential? props) (every? keyword? props)]
  (let [args (vo-constructor-arglist-from-map votrait props)]
    `(if-evals ~'value-map
      (list '.apply '~(companion-object-symbol votrait) ~@args);)
    ;else: Runtime argument; return fn instead
      (~runtime-constructor ~'value-map);)
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
  (let [votrait             (eval votrait-expr)
        macroname           (symbol (.getSimpleName votrait))
        converterfn-name    (symbol (str "to-" (.getSimpleName votrait)))
        runtime-constructor (vo-converterfn-expr votrait converterfn-name)
        macro-body          (defvo-body votrait converterfn-name props)]
    (assert (class? votrait) "votrait must be a VO trait/interface")
    `(binding [*ns* (create-ns '~(symbol (.. votrait getPackage getName)))]
      (eval '~runtime-constructor)
      (eval '(defmacro ~macroname
        ([] ~converterfn-name)
        ([~'value-map] ~macro-body)))
  )))
