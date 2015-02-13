;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.types.http-repository-test
  "The test namespace for the Cassandra file repository."
  (:use [clojure.test]
        [prime.types.http-repository])
  (:require [taoensso.timbre :as log]
            [prime.types.repository-util :as util :refer (exists?)]
            [prime.types.http-repository-util])
  (:import [java.io File]
           [prime.types FileRef]))


(deftest get-file-test
  (testing "retrieving a file via get-file"

    (let [repo (util/mk-repository "http" "")]

      (let [ref (FileRef/apply "https://jenkins.ga.je/static/21bc4dcf/images/headshot.png")]

        (is (exists? repo ref) "it contains the file")

        (is (= (.length ^File (get-file repo ref)) 2409) "it can return the contents as a File")))))
