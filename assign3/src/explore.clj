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


(html5 [:head [:title "ResponseHeader"]] [:body [:ul "here"]])
(defn handler [branches]
  {:status  200                                             ; meaning "OK"
   :headers {"content-type" "text/html"}                    ; instead of e.g. "text/html"
   :body    (html5 [:head [:title "ResponderHeader"]] [:body [:ul branches]])})  ; the payload

(defn start-server [port]
  (run-jetty handler {:port port}))

(defn explore [{:keys [arg dir db]}]
  (let [[switch port] arg]
    (cond
      (or (= switch "-h") (= switch "--help")) (explore-er)
      (not (.exists (io/file (str dir File/separator db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (and (= "-p" switch) (= nil port)) (println "Error: you must specify a numeric port with '-p'.")
      (and (= "-p" switch) (not (>= (try (Integer/parseInt port) (catch NumberFormatException e -1)) 0))) (println "Error: the argument for '-p' must be a non-negative integer.")
      :else (let [port (Integer/parseInt (if (= switch nil) "3000" port))
                  branches (sort (seq (.list (io/file (str dir File/separator db File/separator "refs" File/separator "heads")))))
                  handled (handler branches)]
              (println (format "Starting server on port %d." port))
              (run-jetty handled {:port port})))))
