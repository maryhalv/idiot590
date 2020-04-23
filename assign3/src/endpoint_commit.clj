(ns endpoint_commit
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [hashing])
  (:require [hiccup.page :refer [html5]])
  (:import (java.io File)))

(defn formatAuthor [line]
  (let [half_1 (nth (str/split line #"<") 0)
        half_2 (nth (str/split line #"<") 1)
        half_1_parts (str/split half_1 #" ")
        half_2_parts (str/split half_2 #" ")
        name (if (> (count half_1_parts) 2)
               (str/join " " (vec (rest half_1_parts)))
               (nth half_1_parts 1))
        type (nth half_1_parts 0)
        email (nth (str/split (nth half_2_parts 0) #">") 0)
        timestamp (nth half_2_parts 1)
        timezone (nth half_2_parts 2)]
    [:div {:class "author"} (str type " " name " " "&lt;" email "&gt; " timestamp)]))

(defn formatCommit [line]
  (let [parts (str/split line #" ")]
    (cond
      (= (nth parts 0) "parent") ([:div "parent " [:a {:href (str "/tree/" (nth parts 1))} (nth parts 1)]])
      (= (nth parts 0) "author") (formatAuthor line)
      (= (nth parts 0) "committer") (formatAuthor line)
      (= (count parts) 1) [:pre {:class "message"} line])))

(defn commitBody [object addy]
  (let [commits (str/split object #"\n")
        treeLine (nth commits 0)
        treeAddy (nth (str/split treeLine #" ") 1)]
    (eval (html5 [:head [:title "ResponderHeader"]]
                 [:body
                  [:h1 (str "Commit " addy)]
                  [:div {:class "tree"} "tree " [:a {:href (str "/tree/" treeAddy)} treeAddy]]
                  (map #(formatCommit %) commits)]))))

(defn commitFound [ object addy]
  {:status 200
   :headers {"Content-type" "text/html"}
   :body (commitBody object addy)})

(defn treeFound [addy]
  {:status 302
   :headers {"Location" (str "/tree/" addy)}})

(defn blobFound [addy]
  {:status 302
   :headers {"Location" (str "/blob/" addy)}})

(defn commitEndpoint [dir db addy]
  (let [filepath (hashing/address-conv dir db addy)]
    (if (.exists (io/file filepath))
      (let [object (hashing/bytes->str (second (hashing/split-at-byte 0 (hashing/unzip filepath))))
            object-type (first (str/split (apply str (map hashing/bytes->str (hashing/split-at-byte 0 (hashing/unzip (hashing/address-conv dir db addy))))) #" "))]
        (cond
          (= "blob" object-type) (blobFound addy)
          (= "tree" object-type) (treeFound addy)
          :else (commitFound object addy)))
       {:status 404})))