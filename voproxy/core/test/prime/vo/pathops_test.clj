;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.pathops-test
  (:require [prime.vo.pathops :refer :all]
            [midje.sweet :refer (facts fact)]
            [clojure.test :refer (deftest)]))


(deftest path-operation-tests

  (fact "paths can be filled with variables"

    (fill-path [:booklet :spreads {:id 12} nil nil :id nil]
               [1 2 3 4])
    => [:booklet :spreads {:id 12} 1 2 :id 3])


  (fact "relative vector indexes are calculated correctly"

    (#'prime.vo.pathops/relative-vector-index 3 2)  => 2
    (#'prime.vo.pathops/relative-vector-index 3 4)  => 2
    (#'prime.vo.pathops/relative-vector-index 3 -1) => 2
    (#'prime.vo.pathops/relative-vector-index 3 -3) => 0
    (#'prime.vo.pathops/relative-vector-index 3 -4) => 0
    (#'prime.vo.pathops/relative-vector-index 0 0)  => 0
    (#'prime.vo.pathops/relative-vector-index 3 4 :allow-index-after-last)  => 3
    (#'prime.vo.pathops/relative-vector-index 3 -1 :allow-index-after-last) => 3
    (#'prime.vo.pathops/relative-vector-index 0 2 :allow-index-after-last)  => 0)


  (fact "an inner value can be retrieved using our special paths"

    (get-in-vo {:content [{:id 111 :source "earth"}]} [:content {:id 111} :source])
    => "earth")


  (fact "a value can be appended to an array"

    (append-to-vo {:booklet {:spreads [{:id 1 :tags ["ahoi"]}]}}
                  [:booklet :spreads {:id 1} :tags]
                  "string")
    => {:booklet {:spreads [{:id 1 :tags ["ahoi" "string"]}]}})


  (fact "a value can be inserted in an array as a specific index"

    (insert-at {:booklet {:spreads [{:id 1 :tags [:a :b :c "ahoi"]}]}}
               [:booklet :spreads {:id 1} :tags 1]
               "string")
    => {:booklet {:spreads [{:id 1 :tags [:a "string" :b :c "ahoi"]}]}})


  (fact "a primitive value in an array can be moved to another index"

    (move-vo-to {:booklet {:spreads [{:id 1 :tags ["ahoi" 0 1 2 3 4 5]}]}}
                [:booklet :spreads {:id 1} :tags 0]
                3)
    => {:booklet {:spreads [{:id 1 :tags [0 1 "ahoi" 2 3 4 5]}]}})


  (fact "a value object in an array can be moved to a lower index"

    (move-vo-to {:booklet {:spreads [{:id 1} {:id 2}]}}
                [:booklet :spreads {:id 2}]
                0)
    => {:booklet {:spreads [{:id 2} {:id 1}]}})


  (fact "a value object in an array can be moved to a higher index"

    (move-vo-to {:booklet {:spreads [{:id 1} {:id 2}]}}
                [:booklet :spreads {:id 1}]
                2)
    => {:booklet {:spreads [{:id 2} {:id 1}]}})


  (facts "about replacing values"

    (fact "a value within an array can be replaced"

      (replace-at {:booklet {:spreads [{:id 1 :tags ["ahoi" 0 1 2 3 4 5]}]}}
                  [:booklet :spreads {:id 1} :tags 0]
                  "VET!")
      => {:booklet {:spreads [{:id 1 :tags ["VET!" 0 1 2 3 4 5]}]}})


    (fact "an array value within a VO can be replaced"

      (replace-at {:booklet {:spreads [{:id 1 :tags ["ahoi" 0 1 2 3 4 5]}]}}
                  [:booklet :spreads {:id 1} :tags]
                  ["foo" "bar"])
      => {:booklet {:spreads [{:id 1 :tags ["foo" "bar"]}]}})


    (fact "a non-array value within a VO can be replaced"

      (replace-at {:publication {:booklet {:firstPageOffset 1}}}
                  [:publication :booklet :firstPageOffset]
                  2)
      => {:publication {:booklet {:firstPageOffset 2}}})

    (fact "a value within an array can be replaced with multiple values"

      (replace-at {:booklet {:spreads [{:id 1 :tags [0 1 2 3 4]}]}}
                  [:booklet :spreads {:id 1} :tags 2]
                  ["two" "two" "two"])
      => {:booklet {:spreads [{:id 1 :tags [0 1 "two" "two" "two" 3 4]}]}}))


  (fact "a map can be merged in VO, replacing its values"

    (merge-at {:booklet {:spreads [{:id 1 :tags ["ahoi" 0 1 2 3 4 5]}]}}
              [:booklet :spreads {:id 1}]
              {:id 134})
    => {:booklet {:spreads [{:id 134 :tags ["ahoi" 0 1 2 3 4 5]}]}})


  (fact "a value can be removed from an array"

    (remove-from {:booklet {:spreads [{:id 1 :tags ["ahoi" 0 1 2 3 4 5]}]}}
                 [:booklet :spreads {:id 1} :tags "ahoi"])
    => {:booklet {:spreads [{:id 1 :tags [0 1 2 3 4 5]}]}})


  (facts "about negative indexes"

    (fact "negative indexes work for insert at"

      (insert-at {:tags [1 2 3 4]} [:tags -2] "whoop")
      => {:tags [1 2 "whoop" 3 4]}

      (insert-at {:tags [1 2 3 4]} [:tags -1] "whoop")
      => {:tags [1 2 3 "whoop" 4]})


    (fact "negative indexes work for move to"

      (move-vo-to {:tags [1 2 3 4]} [:tags -2] 0)
      => {:tags [3 1 2 4]}

      (move-vo-to {:tags [1 2 3 4]} [:tags -1] 0)
      => {:tags [4 1 2 3]})


    (fact "negative indexes work for replace at"

      (replace-at {:tags [1 2 3 4]} [:tags -2] "3")
      => {:tags [1 2 "3" 4]}

      (replace-at {:tags [1 2 3 4]} [:tags -1] "4")
      => {:tags [1 2 3 "4"]})

    (fact "negative indexes work for remove from"

      (remove-from {:tags [1 2 3 4]} [:tags -2])
      => {:tags [1 2 4]}

      (remove-from {:tags [1 2 3 4]} [:tags -1])
      => {:tags [1 2 3]})))
