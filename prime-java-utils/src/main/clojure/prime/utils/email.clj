;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.utils.email
  (:require [prime.types :refer (to-EmailAddr)]))

(defn mx-record-count [e-mail]
  (->> (to-EmailAddr e-mail)                     ; Validate email syntax
       str (re-matches #"(?:[^@]+)@(.*)") second ; Get hostname after @
       (prime.utils.MXLookup/doLookup)))         ; Check there's at least one MX record
