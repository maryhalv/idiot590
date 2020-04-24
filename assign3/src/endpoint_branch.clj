(ns endpoint-branch
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [hashing])
  (:require [hiccup.page :refer [html5]])
  (:import (java.io File))
  (:require [log]))


(defn returnLi [address commit-mes-sum]
  (let [linkpart [:a {:href (str "/commit/" (str/trim-newline address))} (subs address 0 7)]
        messpart (str " " (str/trim-newline commit-mes-sum))]
    [:li linkpart messpart]))


(defn logbody [{:keys [dir db branch]}]
  (let [logresult (with-out-str (log/log {:arg (vector "--oneline" branch) :dir dir :db db}))
        entries (clojure.string/split-lines logresult)
        abrev-addresses (map first (map #(str/split % #" ")   entries))
        full-addresses (map :addr (map #(hashing/get-address {:addr % :db db :dir dir}) abrev-addresses))
        commit-mes-sum (map #(subs % 8) entries)]
    [:body
     [:ul {:class "commit-list"} (map #(returnLi (str/trim-newline %1) (str/trim-newline %2)) full-addresses commit-mes-sum)]]
    ))

(defn returnlog [{:keys [dir db branch]}]
  {:status  200
   :headers {"Content-type" "text/html"}
   :body    (html5 [:head [:title "ResponderHeader"]]
                   (logbody {:dir dir :db db :branch branch}))
   }
  )


(defn commitBranch [{:keys [dir db branch]}]
  (let [branch-path (str dir File/separator db File/separator "refs" File/separator "heads" File/separator branch)]
    (cond
      (not (.exists (io/file branch-path))) {:status 404}
      :else (returnlog {:dir dir :db db :branch branch})))
  )
