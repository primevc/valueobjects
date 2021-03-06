;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.proxy.msgpack
  "A concrete implementation of the VOProxy protocol, with MessagePack
  as its backend."
  (:refer-clojure :exclude [update])
  (:require [prime.vo.proxy :refer (VOProxy)]
            [prime.vo.util.msgpack :as mp]))


(deftype MessagePackVOProxy [^String directory]
  VOProxy
  (get-vo [this vo]
    (mp/get directory vo))

  (put-vo [this vo]
    (mp/put directory vo {}))

  (put-vo [this vo options]
    (mp/put directory vo options))

  (update [this vo]
    (mp/update directory vo (:id vo) {}))

  (update [this vo id]
    (mp/update directory vo (or id (:id vo)) {}))

  (update [this vo id options]
    (mp/update directory vo (or id (:id vo)) (or options {})))

  (delete [this vo]
    (mp/delete directory vo {}))

  (delete [this vo options]
    (mp/delete directory vo options)))
