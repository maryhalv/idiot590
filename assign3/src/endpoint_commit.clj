(ns endpoint_commit
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [hashing])
  (:require [hiccup.page :refer [html5]]))

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
    [:div {:class (str type)} (str type " " name " " "&lt;" email "&gt; " timestamp " " (str/trim-newline timezone))]))

(defn formatCommit [line]
  (let [parts (str/split line #" ")]
    (cond
      (= (nth parts 0) "parent") [:div {:class "parent"} "parent " [:a {:href (str "/commit/" (str/trim-newline (nth parts 1)))} (str/trim-newline (nth parts 1))]]
      (= (nth parts 0) "author") (formatAuthor line)
      (= (nth parts 0) "committer") (formatAuthor line)
      (and (not (= line "")) (not (= (nth parts 0) "tree"))) [:pre {:class "message"} line]
      :else nil)))
;;check for more than one pre tag, call function to format those pre tags into one pre
(defn commitBody [object addy]
  (let [commits (str/split object #"\n")
        treeLine (nth commits 0)
        treeAddy (nth (str/split treeLine #" ") 1)
        bodyFormat (map #(formatCommit %) commits)
        pre_test (take-last 3 bodyFormat)
        body_1 (take (- (count bodyFormat) 3) bodyFormat)
        pre_test (apply str pre_test)
        pre_split (nth (str/split pre_test #"]") 0)
        pre_split_message (nth (str/split pre_split #"}") 1)
        message_1 (subs pre_split_message 2 (- (count pre_split_message) 1))
        pre_1 (take 5 pre_split)
        pre_split_2 (nth (str/split pre_test #"]") 1)
        pre_split_2_message (nth (str/split pre_split_2 #"}") 1)
        message_2 (subs pre_split_2_message 2 (- (count pre_split_2_message) 1))
        pre_2 (take 5 pre_split_2)
        body_2 (if (= pre_1 pre_2)
                 [:pre {:class "message"} (str message_1 "\n\n" message_2)]
                 [:pre {:class "message"} (str/trim-newline message_2)])]
    (eval (html5 [:head [:title "ResponderHeader"]]
                 [:body
                  [:h1 (str "Commit " addy)]
                  [:div {:class "tree"} "tree " [:a {:href (str "/tree/" treeAddy)} treeAddy]]
                  body_1
                  body_2]))))

(defn commitFound [object addy]
  {:status  200
   :headers {"Content-type" "text/html"}
   :body (commitBody object addy)})

(defn treeFound [addy]
  {:status 302
   :headers {"Location" (str "/tree/" addy)}})

(defn blobFound [addy]
  {:status 302
   :headers {"Location" (str "/blob/" addy)}})

(defn returnLi [addy dir db first_2]
  (let [full_addy (apply str (concat first_2 addy))
        filepath (hashing/address-conv dir db full_addy)
        object-type (first (str/split (apply str (map hashing/bytes->str (hashing/split-at-byte 0 (hashing/unzip filepath)))) #" "))]
    [:li [:a {:href (str "/" object-type "/" full_addy)} full_addy] (str " (" object-type ")")]))

(defn multipleCommitsBody [addys dir db first_2]
  (eval (html5 [:head [:title "ResponderHeader"]]
               [:body
                [:p "The given address prefix is ambiguous. Please disambiguate your intent by choosing from the following options."]
                [:ul {:class "disambiguation-list"}
                 (map #(returnLi % dir db first_2) addys)]])))

(defn multipleCommits [addys dir db first_2]
  {:status  300
   :headers {"Content-type" "text/html"}
   :body (multipleCommitsBody addys dir db first_2)})

(defn commitEndpoint [dir db addy]
  (let [addy_handler (hashing/get-address-endpoint {:addr addy :db db :dir dir})
        addy_coll (:addr addy_handler)
        is_one (:one addy_handler)]
    (if is_one
      (let [filepath (hashing/address-conv dir db addy_coll)
            addy_full addy_coll]
        (if (.exists (io/file filepath))
          (let  [object (hashing/bytes->str (second (hashing/split-at-byte 0 (hashing/unzip (hashing/address-conv dir db addy_full)))))
                 object-type (first (str/split (apply str (map hashing/bytes->str (hashing/split-at-byte 0 (hashing/unzip (hashing/address-conv dir db addy_full))))) #" "))]
            (cond
              (= (str object-type) "blob") (blobFound addy)
              (= (str object-type) "tree") (treeFound addy)
              :else (commitFound object addy)))
          {:status 404}))
      (multipleCommits addy_coll dir db (take 2 addy)))))