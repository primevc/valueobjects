;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.proxy-tests
  "This namespace contains generic tests, which can be used to test
  actual implementations of VOProxy, VOHistoryProxy and VOSearchProxy.

  Create the proxy and call (test-voproxy your-proxy) within a
  (deftest ...)"
  (:refer-clojure :exclude [update])
  (:require [clojure.test :refer :all]
            [prime.vo.proxy :refer (VOProxy get-vo put-vo update delete append-to insert-at move-to
                                    replace-at merge-at remove-from VOSearchProxy search)]
            [prime.test.vo])
  (:refer prime.test.vo)
  (:import [org.bson.types ObjectId]))


;; (use 'clojure.tools.trace)
;; (trace-vars prime.vo.pathops/update-in-vo prime.vo.pathops/update-with-replace)

[[:chapter {:title "Using this namespace"}]]

"Call the `test-voproxy` function with the VOProxy you want to test."

[[{:title "Setup code"}]]
(def ^{:dynamic true :private true} *proxy-under-test* nil)


(defn test-voproxy
  [proxy]
  (binding [*proxy-under-test* proxy]
    (test-all-vars 'prime.vo.proxy-tests)))

[[:chapter {:title "VOProxy functions"}]]

"This chapter contains the tests for the VOProxy protocol."

[[:section {:title "Simple CRUD operations"}]]

"The following tests are about creating, reading, updating and deleting ValueObjects."

[[{:numbered false}]]
(deftest simple-crud
  (when (satisfies? VOProxy *proxy-under-test*)

    (let [id (ObjectId.)]

      (testing "simple sequence of put-vo and get-vo"
        (let [vo (ValueObjectTest {:id id :name "put-test"})]
          (put-vo *proxy-under-test* vo)
          (is (= vo (get-vo *proxy-under-test* (ValueObjectTest {:id id}))))))

      (testing "replacing a VO with put-vo"
        (let [vo (ValueObjectTest {:id id :name "replace-test"})]
          (put-vo *proxy-under-test* vo)
          (is (= vo (get-vo *proxy-under-test* (ValueObjectTest {:id id}))))))

      (testing "update existing VO using ID from the VO data itself"
        (let [vo (ValueObjectTest {:id id :name "update-test-1" :a "foo"})]
          (update *proxy-under-test* vo)
          (is (= vo (get-vo *proxy-under-test* (ValueObjectTest {:id id}))))))

      (testing "update existing VO using explicit ID"
        (let [vo (ValueObjectTest {:name "update-test-2" :m ["foo" "bar"]})]
          (update *proxy-under-test* vo id)
          (is (= (assoc vo :id id :a "foo")
                 (get-vo *proxy-under-test* (ValueObjectTest {:id id}))))))

      (testing "update existing VO with nested data"
        (let [vo (ValueObjectTest {:name "update-test-3" :owner {:name "pwned"}})]
          (update *proxy-under-test* vo id)
          (is (= (assoc vo :id id :a "foo" :m ["foo" "bar"])
                 (get-vo *proxy-under-test* (ValueObjectTest {:id id}))))))

      (testing "update existing VO with deep nested data"
        (let [vo (ValueObjectTest {:name "update-test-4" :owner {:a [{:name "nested"}]}})]
          (update *proxy-under-test* vo id)
          (is (= (assoc vo :id id :a "foo" :m ["foo" "bar"]
                        :owner {:name "pwned" :a [{:name "nested"}]})
                 (get-vo *proxy-under-test* (ValueObjectTest {:id id}))))))

      (testing "update existing VO with deep nested data after put (Jort's bug)"
        (let [id (ObjectId.)
              vo-put (ValueObjectTest {:meta {:created {:at (.getTime (java.util.Date.))}} :id id})
              vo-upd (ValueObjectTest {:meta {:o (ObjectId.)}})]
          (put-vo *proxy-under-test* vo-put)
          (update *proxy-under-test* vo-upd id)
          (is (-> (get-vo *proxy-under-test* (ValueObjectTest {:id id})) :meta :created :at))))

      (testing "deleting a VO"
        (delete *proxy-under-test* (ValueObjectTest {:id id}))
        (is (nil? (get-vo *proxy-under-test* (ValueObjectTest {:id id})))))

      (testing "nil on getting non-existing VO"
        (is (nil? (get-vo *proxy-under-test* (ValueObjectTest {:id (ObjectId.)}))))))

    (testing "update can upsert a new VO"
      (let [vo (ValueObjectTest {:id (ObjectId.) :name "upsert-test"})]
        (update *proxy-under-test* vo)
        (is (= vo (get-vo *proxy-under-test* vo)))
        (delete *proxy-under-test* vo)))

    (testing "error on put-vo without an ID in the VO"
      (is (thrown? Throwable (put-vo *proxy-under-test* (ValueObjectTest {:name "fail"})))))

    (testing "error on update without an ID in the VO nor an ID specified"
      (is (thrown? Throwable (update *proxy-under-test* (ValueObjectTest {:name "fail"})))))))


[[:section {:title "Appending"}]]

"The following tests are about appending values to various paths in a ValueObject."

[[{:numbered false}]]
(deftest appending
  (when (satisfies? VOProxy *proxy-under-test*)

    (let [vo (ValueObjectTest {:id (ObjectId.) :name "appending"})]
      (put-vo *proxy-under-test* vo)

      (testing "appending to non-existing array"
        (append-to *proxy-under-test* vo [:meta :tags] nil "foo")
        (is (= (assoc-in vo [:meta :tags] ["foo"]) (get-vo *proxy-under-test* vo))))

      (testing "appending to existing array"
        (append-to *proxy-under-test* vo [:meta :tags] nil "bar")
        (is (= (assoc-in vo [:meta :tags] ["foo" "bar"]) (get-vo *proxy-under-test* vo))))

      (testing "appending multiple values to array"
        (append-to *proxy-under-test* vo [:meta :tags] nil ["alice" "bob"])
        (is (= (assoc-in vo [:meta :tags] ["foo" "bar" "alice" "bob"]) (get-vo *proxy-under-test* vo))))

      (testing "error on append to non-array path"
        (is (thrown? Throwable (append-to *proxy-under-test* vo [:name] nil "fail"))))

      (delete *proxy-under-test* vo))))


[[:section {:title "Inserting"}]]

"The following tests are about inserting values to various paths in a ValueObject."

[[{:numbered false}]]
(deftest insterting
  (when (satisfies? VOProxy *proxy-under-test*)

    (let [usage-id (ObjectId.)
          usage (ValueObjectTest {:id usage-id})
          owner (VOTestRef {:a [usage]})
          vo (ValueObjectTest {:id (ObjectId.) :name "inserting" :owner owner})]
      (put-vo *proxy-under-test* vo)

      (testing "inserting to non-existing array"
        (insert-at *proxy-under-test* vo [:meta :tags 0] nil "foo")
        (is (= ["foo"] (-> (get-vo *proxy-under-test* vo) :meta :tags))))

      (testing "inserting at beginning of existing array"
        (insert-at *proxy-under-test* vo [:meta :tags 0] nil "bar")
        (is (= ["bar" "foo"] (-> (get-vo *proxy-under-test* vo) :meta :tags))))

      (testing "inserting at end of existing array"
        (insert-at *proxy-under-test* vo [:meta :tags -1] nil "baz")
        (is (= ["bar" "baz" "foo"] (-> (get-vo *proxy-under-test* vo) :meta :tags))))

      (testing "inserting at out of bounds index succeeds"
        (insert-at *proxy-under-test* vo [:meta :tags 100] nil "alice")
        (is (= ["bar" "baz" "alice" "foo"] (-> (get-vo *proxy-under-test* vo) :meta :tags))))

      (testing "error on inserting at non-array path"
        (is (thrown? Throwable (insert-at *proxy-under-test* vo [:name 0] nil "fail"))))

      (delete *proxy-under-test* vo))))


[[:section {:title "Moving"}]]

"The following tests are about moving values in various array paths in a ValueObject."

[[{:numbered false}]]
(deftest moving
  (when (satisfies? VOProxy *proxy-under-test*)

    (let [vo (IntIDObjectTest {:id 1337 :tags ["foo" "bar" "baz"]})]
      (put-vo *proxy-under-test* vo)

      (testing "moving from front to back"
        (move-to *proxy-under-test* vo [:tags 0] nil 2)
        (is (= ["bar" "baz" "foo"] (:tags (get-vo *proxy-under-test* vo)))))

      (testing "moving from front to back using negative index"
        (move-to *proxy-under-test* vo [:tags 0] nil -1)
        (is (= ["baz" "foo" "bar"] (:tags (get-vo *proxy-under-test* vo)))))

      (testing "moving from front to back using value index"
        (move-to *proxy-under-test* vo [:tags {:= "baz"}] nil -1)
        (is (= ["foo" "bar" "baz"] (:tags (get-vo *proxy-under-test* vo)))))

      (testing "moving from back to front using value index"
        (move-to *proxy-under-test* vo [:tags {:= "baz"}] nil 0)
        (is (= ["baz" "foo" "bar"] (:tags (get-vo *proxy-under-test* vo)))))

      (testing "moving from back to front"
        (move-to *proxy-under-test* vo [:tags 2] nil 0)
        (is (= ["bar" "baz" "foo"] (:tags (get-vo *proxy-under-test* vo)))))

      (testing "moving from back to front using negative index"
        (move-to *proxy-under-test* vo [:tags -1] nil 0)
        (is (= ["foo" "bar" "baz"] (:tags (get-vo *proxy-under-test* vo)))))

      (testing "moving to same index"
        (move-to *proxy-under-test* vo [:tags 0] nil 0)
        (is (= ["foo" "bar" "baz"] (:tags (get-vo *proxy-under-test* vo))))

        (move-to *proxy-under-test* vo [:tags 1] nil -2)
        (is (= ["foo" "bar" "baz"] (:tags (get-vo *proxy-under-test* vo)))))

      (testing "error on moving in empty array"
        (is (thrown? Throwable (move-to *proxy-under-test* vo [:owner :a 0] nil 0))))

      (delete *proxy-under-test* vo))))

(deftest moving-ids
  (when (satisfies? VOProxy *proxy-under-test*)

    (let [vo (IntIDObjectTest {:id 1338 :objects [{:id (ObjectId.) :name "foo"}
                                                  {:id (ObjectId.) :name "bar"}]})
          foo (-> vo :objects first)
          bar (-> vo :objects second)
          foo-id (:id foo)
          bar-id (:id bar)]
      (put-vo *proxy-under-test* vo)

      (testing "moving from front to back"
        (move-to *proxy-under-test* vo [:objects {:id *}] [foo-id] 2)
        (is (= [bar foo] (:objects (get-vo *proxy-under-test* vo)))))

      (testing "moving from front to back using negative index"
        (move-to *proxy-under-test* vo [:objects {:id *}] [bar-id] -1)
        (is (= [foo bar] (:objects (get-vo *proxy-under-test* vo)))))

      (testing "moving from back to front"
        (move-to *proxy-under-test* vo [:objects {:id *}] [bar-id] 0)
        (is (= [bar foo] (:objects (get-vo *proxy-under-test* vo)))))

      (testing "moving to same index"
        (move-to *proxy-under-test* vo [:objects {:id *}] [bar-id] 0)
        (is (= [bar foo] (:objects (get-vo *proxy-under-test* vo))))

        (move-to *proxy-under-test* vo [:objects {:id *}] [foo-id] 1)
        (is (= [bar foo] (:objects (get-vo *proxy-under-test* vo)))))

      (delete *proxy-under-test* vo))))


[[:section {:title "Replacing"}]]

"The following tests are about replacing values in various paths in a ValueObject."

[[{:numbered false}]]
(deftest replacing
  (when (satisfies? VOProxy *proxy-under-test*)

    (let [vo (IntIDObjectTest {:id 1339 :name "replace-test" :tags ["foo" "bar"]})]
      (put-vo *proxy-under-test* vo)

      (testing "replacing a normal value"
        (replace-at *proxy-under-test* vo [:name] nil "replaced")
        (is (= "replaced" (:name (get-vo *proxy-under-test* vo)))))

      (testing "replacing a value inside an array, by index"
        (replace-at *proxy-under-test* vo [:tags -1] nil "bup")
        (is (= ["foo" "bup"] (:tags (get-vo *proxy-under-test* vo)))))

      (testing "replacing a value inside an array, by value"
        (replace-at *proxy-under-test* vo [:tags {:= "bup"}] nil "baz")
        (is (= ["foo" "baz"] (:tags (get-vo *proxy-under-test* vo)))))

      (testing "replacing a value inside an array with multiple values"
        (replace-at *proxy-under-test* vo [:tags 0] nil ["whoop" "whoop"])
        (is (= ["whoop" "whoop" "baz"] (:tags (get-vo *proxy-under-test* vo)))))

      (testing "replacing an array value"
        (replace-at *proxy-under-test* vo [:tags] nil ["alice"])
        (is (= ["alice"] (:tags (get-vo *proxy-under-test* vo)))))

      (testing "replacing non-set value"
        (replace-at *proxy-under-test* vo [:a] nil "new")
        (is (= "new" (:a (get-vo *proxy-under-test* vo)))))

      (testing "error on replacing a not reachable path"
        (is (thrown? Throwable (replace-at *proxy-under-test* vo [:owner :usage 1] nil
                                           (ValueObjectTest {:id (ObjectId.)})))))

      (delete *proxy-under-test* vo))))


[[:section {:title "Merging"}]]

"The following tests are about merging values in various paths in a ValueObject."

[[{:numbered false}]]
(deftest merging
  (when (satisfies? VOProxy *proxy-under-test*)

    (let [vo (ValueObjectTest {:id (ObjectId.) :name "merge-test" :meta {:tags ["foo" "bar"]}
                           :owner (VOTestRef {:name "pwned"})})]
      (put-vo *proxy-under-test* vo)

      (testing "merging a value inside a VO"
        (merge-at *proxy-under-test* vo [:owner] nil
                  (VOTestRef {:name "merged" :a [(ValueObjectTest {:id (ObjectId.)})]}))
        (is (= "merged" (-> (get-vo *proxy-under-test* vo) :owner :name)))
        (is (-> (get-vo *proxy-under-test* vo) :owner :a)))

      (testing "merging to empty path, having :allow-nil-or-empty-path? set to true"
        (merge-at *proxy-under-test* vo nil nil (ValueObjectTest {:name "merged"})
                  {:allow-nil-or-empty-path? true})
        (is (= "merged" (:name (get-vo *proxy-under-test* vo)))))

      (testing "error on merging to empty path default option"
        (is (thrown? Throwable (merge-at *proxy-under-test* vo nil nil "fail"))))

      (testing "error on merging to empty path, having :allow-nil-or-empty-path? set to false"
        (is (thrown? Throwable (merge-at *proxy-under-test* vo nil nil "fail"
                                         {:allow-nil-or-empty-path? false}))))

      (testing "error on merging to non-VO path"
        (is (thrown? Throwable (merge-at *proxy-under-test* vo [:name] nil "fail"))))

      (delete *proxy-under-test* vo))))


[[:section {:title "Removing"}]]

"The following tests are about removing values in various paths in a ValueObject."

[[{:numbered false}]]
(deftest removing
  (when (satisfies? VOProxy *proxy-under-test*)

    (let [uid-1 (ObjectId.)
          uid-2 (ObjectId.)
          vo (ValueObjectTest {:id (ObjectId.) :name "remove-test" :meta {:tags ["foo" "bar" "baz"]}
                           :owner {:name "pwned" :a [{:id uid-1} {:id uid-2}]}})]
      (put-vo *proxy-under-test* vo)

      (testing "removing values"
        (remove-from *proxy-under-test* vo [:name] nil)
        (remove-from *proxy-under-test* vo [:meta :tags 1] nil)
        (remove-from *proxy-under-test* vo [:meta :tags {:= "baz"}] nil)
        (remove-from *proxy-under-test* vo [:owner :a {:id uid-2}] nil)
        (is (= (get-vo *proxy-under-test* vo)
               (-> vo
                   (dissoc :name)
                   (assoc-in [:meta :tags] ["foo"])
                   (assoc :owner (VOTestRef {:a [{:id uid-1}] :name "pwned"}))))))

      (testing "removing non-set path is fine"
        (is (or (remove-from *proxy-under-test* vo [:r] nil) true)))

      (testing "removing in primitive arrays by value must use {:= ...} map"
        (is (thrown? Throwable (remove-from *proxy-under-test* vo [:meta :tags {:id "foo"}] nil))))

      (delete *proxy-under-test* vo))

    (let [order-id-1 (ObjectId.)
          order-id-2 (ObjectId.)
          vo (IntIDObjectTest {:id 1340
                               :members [{:id 1 :name "Alice"}
                                         {:id 2 :name "Bob"}]
                               :objrefs [{:id order-id-1 :name "20150001"}
                                         {:id order-id-2 :name "20150002"}]})]
      (put-vo *proxy-under-test* vo)

      (testing "removing in ref (integer) arrays"
        (remove-from *proxy-under-test* vo [:members {:= "1"}] nil)
        (is (= (->> (get-vo *proxy-under-test* vo) :members (map #(._id %)))
               [2])))

      (testing "removing in ref (id) arrays"
        (remove-from *proxy-under-test* vo [:objrefs {:= (str order-id-1)}] nil)
        (is (= (->> (get-vo *proxy-under-test* vo) :objrefs (map #(._id %)))
               [order-id-2]))))))


[[:chapter {:title "VOSearchProxy functions"}]]

"This chapter contains the tests for the VOSearchProxy protocol."

[[:section {:title "Searching"}]]

"The following tests are about searching ValueObjects."

[[{:numbered false}]]
(deftest searching
  (when (satisfies? VOSearchProxy *proxy-under-test*)

    (let [id1 (ObjectId.)
          vo1 (ValueObjectTest {:id id1
                            :name "first"
                            :meta {:tags ["foo" "bar"]}
                            :owner (VOTestRef {:name "pwned"})})
          usage (ValueObjectTest {:id (ObjectId.)})
          owner (VOTestRef {:name "Greet" :a [usage]})
          vo2 (ValueObjectTest {:id (ObjectId.)
                            :name "second"
                            :meta {:tags ["foo" "bar" "baz"]}
                            :owner owner})
          vo3 (ValueObjectTest {:id (ObjectId.)
                            :name "third"
                            :owner {:a [{:created {:id 1}}
                                        {:created {:id 2}}]}})]
      (put-vo *proxy-under-test* vo1)
      (put-vo *proxy-under-test* vo2)
      (put-vo *proxy-under-test* vo3)

      (testing "search by id"
        (let [result (search *proxy-under-test* (ValueObjectTest {:id id1}))]
          (is (= 1 (count result)))
          (is (= vo1 (first result)))))

      (testing "search by direct value"
        (let [result (search *proxy-under-test* (ValueObjectTest {:name "second"}))]
          (is (= 1 (count result)))
          (is (= vo2 (first result)))))

      (testing "search by nested value"
        (let [result (search *proxy-under-test* (ValueObjectTest {:owner (VOTestRef {:name "pwned"})}))]
          (is (= 1 (count result)))
          (is (= vo1 (first result)))))

      (testing "search by value in array"
        (is (= 2 (count (search *proxy-under-test* (ValueObjectTest {:meta {:tags ["foo"]}})))))
        (is (= 1 (count (search *proxy-under-test* (ValueObjectTest {:meta {:tags ["baz"]}})))))
        (is (= 2 (count (search *proxy-under-test* (ValueObjectTest {:meta {:tags ["foo" "bar"]}})))))
        (is (= 1 (count (search *proxy-under-test* (ValueObjectTest {:meta {:tags ["foo" "bar" "baz"]}})))))
        (is (= vo2 (first (search *proxy-under-test* (ValueObjectTest {:owner {:a [usage]}}))))))

      (testing "search by inner values in array"
        (is (= 1 (count (search *proxy-under-test*
                                (ValueObjectTest {:owner {:a
                                                      [{:created {:id 1}}]}})))))
        (is (= 1 (count (search *proxy-under-test*
                                (ValueObjectTest {:owner {:a
                                                      [{:created {:id 1}}
                                                       {:created {:id 2}}]}})))))
        (is (= 0 (count (search *proxy-under-test*
                                (ValueObjectTest {:owner {:a
                                                      [{:created {:id 1}}
                                                       {:created {:id 1337}}]}}))))))


      (testing "searching yields no results"
        (is (= 0 (count (search *proxy-under-test* (ValueObjectTest {:name "nope"}))))))

      (delete *proxy-under-test* vo1)
      (delete *proxy-under-test* vo2))))
