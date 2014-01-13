;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.utils.timbre
  "Load this namespace to have timbre logging over tools.logging."
  (:require [taoensso.timbre :as timbre]))


(try
  (require 'clojure.tools.logging)
  (let [tools-logging-appender
        (quote {:doc "Forwards logging messages to tools.logging."
                :enabled? true
                :async? false
                :fn (fn [{:keys [level message throwable ns]}]
                      (clojure.tools.logging/log ns level throwable message))})]
    (timbre/set-config! [:appenders :standard-out :enabled?] false)
    (timbre/set-config! [:appenders :tools.logging] (eval tools-logging-appender))
    (timbre/info "Standard out Timbre logging now forwarded to tools.logging."))
  (catch java.lang.ClassNotFoundException ex
    (println (str "prime.utils.timbre: Could not load tools.logging, make sure "
                  "tools.logging is on your classpath."))))
