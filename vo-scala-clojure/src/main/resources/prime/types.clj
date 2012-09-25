;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.types)

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

(defprotocol FileRepository
  (^FileRefOutputStream create [this])
  (^Boolean             exists [this ^FileRef ref  ])
  (^URI                 toURI  [this ^FileRef ref  ])
  (^FileRef             absorb [this ^File    file ])
  (^InputStream         stream [this ^FileRef ref  ])

  (^FileRef             store  [this writer]))

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
