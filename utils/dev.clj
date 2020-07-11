(ns dev
  (:require [common :as common]))

(defonce server (common/run-server 3000 false true))
