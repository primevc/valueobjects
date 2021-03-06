;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(set! *warn-on-reflection* true)

(ns prime.types
  (:import org.bson.types.ObjectId
           java.net.URI, java.net.URL
           javax.mail.internet.InternetAddress
           scala.collection.immutable.IndexedSeq
          [java.io File InputStream]
          [java.nio ByteBuffer]
          [prime.types Conversion VORef]
          [org.joda.time DateMidnight DateTime Interval ReadableInstant]
          [org.joda.time.format DateTimeFormatter])
  (:use [clojure.lang]))

(defprotocol To-ObjectId  (^ObjectId        ^:pure  to-ObjectId  [in]                            ))
(defprotocol To-String    (^String          ^:pure  to-String    [in] [in format]                ))
(defprotocol To-Boolean   (^Boolean         ^:pure  to-Boolean   [in]                            ))
(defprotocol To-Integer   (^Integer         ^:pure  to-Integer   [in]                            ))
(defprotocol To-Long      (^Long            ^:pure  to-Long      [in]                            ))
(defprotocol To-Decimal   (^Double          ^:pure  to-Decimal   [in] [in format]                ))
(defprotocol To-RGBA      (^RGBA            ^:pure  to-RGBA      [in] [rgb a]                    ))
(defprotocol To-Date      (^DateMidnight    ^:pure  to-Date      [in] [in formatter]             ))
(defprotocol To-DateTime  (^DateTime        ^:pure  to-DateTime  [in] [in formatter]             ))
(defprotocol To-Interval  (^Interval        ^:pure  to-Interval  [in] [start end]                ))
(defprotocol To-EmailAddr (^InternetAddress ^:pure  to-EmailAddr [in]                            ))
(defprotocol To-URI       (^java.net.URI    ^:pure  to-URI       [in]                            ))
(defprotocol To-FileRef   (^FileRef         ^:pure  to-FileRef   [in]                            ))
(defprotocol To-Vector    (^IndexedSeq      ^:pure  to-Vector    [in converter]                  ))
(defprotocol To-VORef     (^VORef                   to-VORef     [in] [in ref-target--companion] ))

(defn ^URI ^:pure to-URL [uri] (Conversion/URL (to-URI uri)))

(extend-type prime.vo.ID To-VORef
  (^VORef to-VORef [in] (prime.types.Conversion/vo2ref in)))

;
; Default dispatch to scala prime.types.Conversion.* methods
;

(defmacro def-default-converter [target-type => & types]
  ; We help the Clojure compiler by tagging everything, also prevents stack-overflows.
  (let [tag       {:tag (eval (first types))}
        converter (with-meta (symbol (.getName Conversion) (name target-type)) tag)
        fn-sym    (with-meta (symbol (str "to-" target-type))                  tag)]
  `(extend-protocol
    ~(symbol "prime.types" (str "To-" target-type))
    ~@(mapcat #(let [class (eval %) in (with-meta 'in {:tag class})]
        (list class
          `(~fn-sym
            ([~in]           (~converter ~in))
            ([~in ~'ignored] (~converter ~in)))))
        (cons nil types)))))

(def ByteArray (Class/forName "[B"))

(def-default-converter Boolean    =>  Boolean          String          Number                    )
(def-default-converter String     =>  String         ObjectId  URI     Number Double Long Integer)
(def-default-converter Integer    =>  Integer          String          Number Double Long        )
(def-default-converter Long       =>  Long             String          Number Double      Integer)
(def-default-converter Decimal    =>  Double                           Number Double Long Integer)
(def-default-converter RGBA       =>  prime.types.RGBA String)
(def-default-converter Date       =>  DateMidnight     DateTime        Number        Long Integer java.util.Date)
(def-default-converter DateTime   =>  DateTime         ReadableInstant Number        Long Integer java.util.Date)
(def-default-converter Interval   =>  Interval)
(def-default-converter EmailAddr  =>  InternetAddress  String  URI  URL)
(def-default-converter URI        =>  URI              String       URL  ObjectId)
(def-default-converter ObjectId   =>  ObjectId         String  URI       ByteBuffer  ByteArray)

(extend-type nil
  To-String
  (^String to-String
    ([in] "")
    ([in ^String format] "")))

(extend-type Object
  To-String
  (^String to-String
    ([in] (.toString in))
    ([in ^String format] (.toString in))))

; Default formatted String converters
(extend-type Double To-String
  (^String to-String
    ([in]                           (Conversion/String   in nil))
    ([in ^String format]            (Conversion/String   in format))))

(extend-type String To-Decimal
 (^Double to-Decimal
    ([in]                           (Conversion/Decimal  in))
    ([in ^String format]            (Conversion/Decimal  in format))))

(extend-type String To-Date
    (^DateMidnight to-Date
    ([in]                           (Conversion/Date     in))
    ([in ^DateTimeFormatter format] (Conversion/Date     in format))))

(extend-type String To-DateTime
  (^DateMidnight to-DateTime
    ([in]                           (Conversion/DateTime in))
    ([in ^DateTimeFormatter format] (Conversion/DateTime in format))))


; Default Interval constructor
(extend-type ReadableInstant To-Interval
  (^Interval to-Interval
    ([start ^ReadableInstant end]   (Conversion/Interval start end))))

; Default `RGB with alpha` constructors
(let [impl `( ([in#]                (Conversion/RGBA in#))
              ([rgb# alpha#]        (if (integer? alpha#)
                                      (Conversion/RGBA rgb# (int    alpha#))
                                    #_else
                                      (Conversion/RGBA rgb# (double alpha#)))))]
  (eval `(extend-protocol To-RGBA
    Integer (^prime.types.RGBA to-RGBA ~@impl)
    Long    (^prime.types.RGBA to-RGBA ~@impl))))


; Clojure types to Vector
(extend-type clojure.lang.ISeq
  ; FIXME: Fully Realizes a Seq and creates a Scala Vector, which might be unexpected behaviour for assoc.
  ;        - Should wrap seq instead and create a vector lazily. (or should it?)
  To-Vector (to-Vector [in converter]
    (if (empty? in)
      (scala.collection.immutable.Vector/empty)
    #_else (.toIndexedSeq (scala.collection.JavaConversions/asScalaIterator
      (clojure.lang.SeqIterator. (map #(invoke converter %) in)))))))

;
; Additional types and abstractions
;

(def ^:pure FileRef prime.types.FileRef$/MODULE$)
(def ^:pure RGBA    to-RGBA)



(comment
;
;  Many composite types like URI, FileRef, ObjectId and Date could become VOs eventually.
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

;
; Clojure standard library pure functions:

(doseq [purefn [
    #'+ #'- #'* #'/ #'+' #'-' #'*'
    #'assoc #'dissoc
    #'bit-and #'bit-and-not #'bit-clear #'bit-flip #'bit-not #'bit-or #'bit-set #'bit-shift-left #'bit-shift-right #'bit-test #'bit-xor
    #'conj
    #'dec #'dec' #'even? #'inc #'inc' #'max #'min #'odd? #'rem
    #'boolean #'short #'int #'long #'bigint #'double #'float
  ]] (alter-meta! purefn assoc :pure true))

;
; Convertable to equality protocol, mainly for pathops
;

(defprotocol ConvertableToEquality (^boolean ^:pure to= [a b]))

(defmacro implement-to= [type conversion-fn]
  `(extend-protocol ConvertableToEquality
    ~type
    (to= [~'a ~'b] (= ~'a (try (~conversion-fn ~'b) (catch Exception ~'e))))))

(implement-to= ObjectId            to-ObjectId)
(implement-to= String              to-String)
(implement-to= Boolean             to-Boolean)
(implement-to= Integer             to-Integer)
(implement-to= Long                to-Long)
(implement-to= Double              to-Decimal)
(implement-to= prime.types.RGBA    to-RGBA)
(implement-to= DateMidnight        to-Date)
(implement-to= DateTime            to-DateTime)
(implement-to= Interval            to-Interval)
(implement-to= InternetAddress     to-EmailAddr)
(implement-to= java.net.URI        to-URI)
(implement-to= prime.types.FileRef to-FileRef)

(extend-protocol prime.types/ConvertableToEquality
  prime.types.VORef
  (to= [a b]
    (to= (._id a) b)))
