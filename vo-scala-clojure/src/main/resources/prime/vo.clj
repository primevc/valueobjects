;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo)

(def ^:dynamic *proxy-map* "
  A function from VOType => VOProxy. Used to fetch a ValueObject when dereferencing VORef properties.
  Initially {}

  Usage:
    (binding [*proxy-map* { MyReferencedValueObject (my-valueobject-proxy ...) }] @(:voref-field vo))
  " {})
