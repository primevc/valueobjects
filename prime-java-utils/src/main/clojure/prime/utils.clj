;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.utils)

(defmacro mapify
  "Given some symbols, construct a map with the symbols as keys, and the value
  of the symbols as the map values. Unbound symbols or those with nil values are
  not included in the resulting map. An example:

  (let [foo \"bar\"]
    (mapify foo))    ; => {:foo \"bar\"}"
  [& symbols]
  `(into {} (filter second ~(into [] (for [item symbols] [(keyword item) item])))))
