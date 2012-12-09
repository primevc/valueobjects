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

(def ^:dynamic *voseq-key-fn* "
  A function from ValueObjectField => Any, or nil.
  When not nil, creating a seq from ValueObject will call this function to produce the key value.
  Initially nil.

  Usage example:
    (binding [*voseq-key-fn* #(.id ^ValueObjectField %)] (map name vo))
  " nil)
