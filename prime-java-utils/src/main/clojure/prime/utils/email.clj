;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.utils.email)

(def vo-enabled?
  (try
    (require 'prime.types)
    true
    (catch Throwable _ false)))

(defn- validate-email [input]
  (if vo-enabled? ((ns-resolve 'prime.types 'to-EmailAddr) input)
   #_else (javax.mail.internet.InternetAddress. (str input))))

(defn mx-record-count [e-mail]
  (->> (validate-email e-mail)                   ; Validate email syntax
       str (re-matches #"(?:[^@]+)@(.*)") second ; Get hostname after @
       (prime.utils.MXLookup/doLookup)))         ; Check there's at least one MX record
