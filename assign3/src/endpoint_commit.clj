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
      :else (if (and (not (= line "")) (not (= (nth parts 0) "tree")))
              [:pre {:class "message"} (str/trim-newline line)]))))

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
  {:status  200
   :headers {"Content-type" "text/html"}
   :body (commitBody object addy)})

(defn treeFound [addy]
  {:status 302
   :headers {"Location" (str "/tree/" addy)}})

(defn blobFound [addy]
  {:status 302
   :headers {"Location" (str "/blob/" addy)}})

(defn returnLi [addy dir db]
  (let [filepath (hashing/address-conv dir db addy)
        object-type (first (str/split (apply str (map hashing/bytes->str (hashing/split-at-byte 0 (hashing/unzip filepath)))) #" "))]
    [:li [:a {:href (str "/" object-type "/" addy)} addy] (str " (" object-type ")")]))

(defn multipleCommitsBody [addys dir db]
  (eval (html5 [:head [:title "ResponderHeader"]]
               [:body
                [:p "The given address prefix is ambiguous. Please disambiguate your intent by choosing from the following options."]
                [:ul {:class "disambiguation-list"}
                 ;; (map #(returnLi (str/trim-newline %) dir db) addys)
                 ]])
        ))

;;addys here should be the full address
(defn multipleCommits [addys dir db]
  {:status  300
   :headers {"Content-type" "text/html"}
   :body (multipleCommitsBody addys dir db)})

(defn commitEndpoint [dir db addy]
  (let [addy_handler (hashing/get-address-endpoint {:addr addy :db db :dir dir})
        addy_coll (:addr addy_handler)
        is_one (:one addy_handler)]
    ;;the address used NEEDS to be the one returned form addy_handler so that it is the full address, not addy
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
      (multipleCommits addy_coll dir db))))