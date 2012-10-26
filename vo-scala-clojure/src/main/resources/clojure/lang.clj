;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns clojure.lang)

(defprotocol IFn
  (applyTo [^clojure.lang.ISeq arglist])
  (invoke
          [ifn]
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
          [ifn a b c d e f g h i j k l m n o p q & r]))


;
; Scala implementation
;
(extend-type scala.runtime.AbstractFunction1
  IFn
    (invoke [f a] (.apply f a)))
