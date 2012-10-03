;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.types
  (:use [clojure.lang]))

(defprotocol To-UniqueID  (to-UniqueID  [in]))
(defprotocol To-String    (to-String    [in] [in format]))
(defprotocol To-Boolean   (to-Boolean   [in]))
(defprotocol To-Integer   (to-Integer   [in]))
(defprotocol To-Decimal   (to-Decimal   [in] [in format]))
(defprotocol To-RGBA      (to-RGBA      [in]))
(defprotocol To-Date      (to-Date      [in] [in formatter]))
(defprotocol To-DateTime  (to-DateTime  [in] [in formatter]))
(defprotocol To-Interval  (to-Interval  [in]))
(defprotocol To-EmailAddr (to-EmailAddr [in]))
(defprotocol To-URI       (to-URI       [in]))
(defprotocol To-FileRef   (to-FileRef   [in]))
(defprotocol To-VORef     (to-VORef     [in ref-target--companion]))
(defprotocol To-Vector    (to-Vector    [in converter]))

(extend-type scala.runtime.AbstractFunction1
  IFn
    (invoke [f a] (.apply f a)))

(extend-type clojure.lang.ISeq
  To-Vector (to-Vector [in converter]
    (if (empty? in)
      (scala.collection.immutable.Vector/empty)
    #_else (.toIndexedSeq (scala.collection.JavaConversions/asScalaIterator
      (clojure.lang.SeqIterator. (map #(invoke converter %) in)))))))

(defprotocol FileRepository
  (^FileRefOutputStream create [this])
  (^Boolean             exists [this ^FileRef ref  ])
  (^URI                 toURI  [this ^FileRef ref  ])
  (^FileRef             absorb [this ^File    file ])
  (^InputStream         stream [this ^FileRef ref  ])

  (^FileRef             store  [this writer]))

(def FileRef prime.types.FileRef$/MODULE$)

(comment
;
;  Many composite types like URI, FileRef, UniqueID and Date could become VOs eventually.
;
;  Converting Strings directly to these types requires something like this:

  (def String-ValueSource-builder {
    1 (ObjectIdStringParser.)
    2 (FileRefStringParser.)
     })

  (extend-type java.lang.String
    ValueSourceable
    (as-source [string target-VO-ID] (as-source (String-ValueSource-builder target-VO-ID) string)))

)
