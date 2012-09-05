;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.definition
  (:use prime.vo.printer, trammel.core)
  (:import (prime.vo ValueObject)))


(defn companion-object-symbol [^Class votrait]
  (symbol (str (.getName votrait) "$") "MODULE$"))

(defn manifest-object-symbol [^Class votrait]
  (symbol (.. (eval (companion-object-symbol votrait)) manifest getClass getName) "MODULE$"))

(defmacro default-value [votrait prop]
  `(.. ~(manifest-object-symbol (eval votrait)) ~prop ~'defaultValue))

(defn vo-constructor-expr [^Class votrait values]
  `(.apply ~(companion-object-symbol votrait) ~@values))

(defn vo-converterfn-expr [^Class votrait fn-name]
  `(defn ~fn-name 
    ([value#] (.apply ~(companion-object-symbol votrait) value#))))

(defn literal-value?
  "Returns true  for literal values like booleans, strings and numbers.
           false for ValueObjects and vectors."
  [value]
  (not (or (instance? java.lang.Iterable   value)
           (instance? ValueObject value))))

(defn default-value-expr [^Class votrait ^clojure.lang.Keyword prop]
  (let [value-expr `(default-value ~votrait ~(.sym prop))
        value       (eval value-expr)]
    (if (literal-value? value)
      value
    ; literal, return the path to the default value to prevent garbage objects
      value-expr)))

(defn vo-constructor-arglist-from-map [^Class votrait value-map & props]
  (map #(if-let [v (% value-map)] v (default-value-expr votrait %)) props))

(defn eval? [expr]
  "Returns the result of `(eval expr)` or false if any Exception is thrown."
  (try (eval expr)
    (catch Exception e#
      ;(do (println "evals=false" e#)
          false)));)

(defmacro if-evals 
  ([expr then]      `(when (eval? ~expr) ~then))
  ([expr then else] `(if   (eval? ~expr) ~then ~else)))

(defconstrainedfn defvo-body
  [^Class votrait runtime-constructor props] [(class? votrait) (symbol? runtime-constructor)
                                              (sequential? props) (every? keyword? props)
                                              =>
                                              list?]
  (let [value-map (gensym "value-map")]
    (list [value-map]
        (list `if-evals value-map
          `;(do (println "compile time constructor")
            (vo-constructor-expr ~votrait (vo-constructor-arglist-from-map ~votrait ~value-map ~@props));)
          ; Runtime map: return fn instead
          `;(do (println "runtime constructor")
            `(~~runtime-constructor ~~value-map);)
        ))))


;
; VO definition macro
;

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
        runtime-constructor (vo-converterfn-expr votrait converterfn-name)]
    (assert (class? votrait) "votrait must be a VO trait/interface")
    `(binding [*ns* (create-ns '~(symbol (.. votrait getPackage getName)))]
      (eval '~runtime-constructor)
      (eval '(defmacro ~macroname
        ([] ~converterfn-name)
        ~(defvo-body votrait converterfn-name props)))
  )))
