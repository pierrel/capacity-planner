(ns web.js.repl
  (:require [weasel.repl :as repl]))

(defonce conn
  (repl/connect "ws://localhost:9001"))
