;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.utils.timbre
  "Load this namespace to have timbre logging over tools.logging."
  (:require [taoensso.timbre :as timbre]
            [clojure.tools.logging :as logging]))


(def tools-logging-appender
  {:doc "Forwards logging messages to tools.logging."
   :enabled? true
   :async? false
   :fn (fn [{:keys [level message throwable ns]}]
         (logging/log ns level throwable message))})

(timbre/set-config! [:appenders :standard-out :enabled?] false)

(timbre/set-config! [:appenders :tools.logging] tools-logging-appender)
