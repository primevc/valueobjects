;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns clojure.lang)

(defn invoke [fnobj arg]
  (if (isa? (class fnobj) clojure.lang.IFn)
    (fnobj arg)
  #_else
    (.apply ^scala.runtime.AbstractFunction1 fnobj arg)))
