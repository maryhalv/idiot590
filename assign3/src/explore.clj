(ns explore
  (:require [clojure.java.io :as io])
  (:import (java.io File))
  (:require [ring.adapter.jetty :refer [run-jetty]])
  (:require [hiccup.page :refer [html5]]))

(defn explore-er []
  (println "idiot explore: start a web server to explore the database

Usage: idiot explore [-p <port>]

Arguments:
   -p <port>   listen on the given port (default: 3000)"))

(defn macro-handler [body]
  (fn handler [_] {:status  200                             ; meaning "OK"
                   :headers {"content-type" "text/html"}    ; instead of e.g. "text/html"
                   :body    body}))

(defn parse [int]
  (try (Integer/parseInt int) (catch NumberFormatException e (.getMessage e))))

(defn explore [{:keys [arg dir db]}]
  (let [[switch port] arg]
    (cond
      (or (= switch "-h") (= switch "--help")) (explore-er)
      (not (.exists (io/file (str dir File/separator db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (and (= "-p" switch) (= nil port)) (println "Error: you must specify a numeric port with '-p'.")
      (and (= "-p" switch) (not= Integer (type (parse port)))) (println "Error: the argument for '-p' must be a non-negative integer.")
      (and (= "-p" switch) (< (parse port) 0)) (println "Error: the argument for '-p' must be a non-negative integer.")
      :else (do
              (cond
                (= nil port) (println "Starting server on port 3000.")
                :else (println (format "Starting server on port %s." port)))
              (let [body (eval (html5 [:head [:title "ResponderHeader"]]
                                      [:body [:ul (sort (seq (.list (io/file (str dir File/separator db File/separator "refs" File/separator "heads")))))]]))
                    port (if (= nil port) 3000 (parse port))]
                (run-jetty (macro-handler body) {:port port}))))))
