;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.source)

(defprotocol ValueSource
  (containsKey [this, name, idx])

  (boolAt   [this, name, idx] [this, name, idx, notFound])
  (intAt    [this, name, idx] [this, name, idx, notFound])
  (doubleAt [this, name, idx] [this, name, idx, notFound])
  (anyAt    [this, name, idx] [this, name, idx, notFound]))

(defprotocol ValueSourceable
  (as-source [this] [this valueobject-definition]))
