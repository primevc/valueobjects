;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.hash
  (import [javax.crypto Mac]
          [javax.crypto.spec SecretKeySpec]))

;TODO: Keep a LRU cache of SecretKeySpecs

(defn hmac-sha1->base64
  "Calculate Base64 encoded HMAC-SHA1 signature for given data."
  ([^String key ^String data]
    (hmac-sha1->base64 key data false))

  ([^String key ^String data ^Boolean url-safe?]
    (let [secret (SecretKeySpec. (.getBytes key "UTF-8") "HmacSHA1")
          mac    (doto (Mac/getInstance "HmacSHA1") (.init secret))
          hash   (.doFinal mac (.getBytes data))
          base64 (org.apache.commons.codec.binary.Base64/encodeBase64 hash false url-safe?)]
      (String. base64 "US-ASCII"))))
