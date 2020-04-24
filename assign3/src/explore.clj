(ns explore
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:import (java.io File))
  (:require [ring.adapter.jetty :refer [run-jetty]])
  (:require [hiccup.page :refer [html5]])
  (:require [endpoint_commit])
  (:require [endpoint-branch]))

(defn explore-er []
  (println "idiot explore: start a web server to explore the database

Usage: idiot explore [-p <port>]

Arguments:
   -p <port>   listen on the given port (default: 3000)"))

;(defn macro-handler [body]
;  (fn handler [_] {:status  200                             ; meaning "OK"
;                   :headers {"content-type" "text/html"}    ; instead of e.g. "text/html"
;                   :body    body}))

(defn returnLi [branch]
  [:li [:a {:href (str "/branch/" (str/trim-newline branch))} (str/trim-newline branch)]])

(defn headBody [dir db]
  (let [file (str dir File/separator db File/separator "HEAD")
        headContent (slurp (io/file file))
        refParse (str/split headContent #" ")
        ref (str/trim-newline (nth (str/split (nth refParse 1) #"/") 2) )
        branches (sort (seq (.list (io/file (str dir File/separator db File/separator "refs" File/separator "heads")))))]
    (eval (html5 [:head [:title "ResponderHeader"]]
                 [:body
                  [:div {:class "head-info"} "HEAD points to ref " [:a {:href (str "/branch/" (str/trim-newline ref))} (str/trim-newline ref)]]
                  [:ul {:class "branch-list"} (map #(returnLi (str/trim-newline %)) branches)]]))))

(defn headEndpoint [dir db]
  {:status 200
   :headers {"Content-type" "text/html"}
   :body (headBody dir db)})

(defn macro-handler [dir db]
  (fn handler [request]
    (let [{:keys [request-method uri]} request
          split-uri (str/split uri #"/")]
      (cond
        (= uri "/") (headEndpoint dir db)
        (= (nth split-uri 1) "branch") (endpoint-branch/commitBranch {:branch (nth split-uri 2) :dir dir :db db})
        (= (nth split-uri 1) "commit") (endpoint_commit/commitEndpoint dir db (nth split-uri 2))
        :else {:status 200
               :headers {"Content-type" "text/html"}
               :body (pr-str {:request-method request-method, :path uri})}))))

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
                (run-jetty (macro-handler dir db) {:port port}))))))
