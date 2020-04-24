(ns endpoint_tree
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [hashing])
  (:require [hiccup.page :refer [html5]])
  (:require [endpoint_commit])
  (:require [commit-tree]))

(defn treeEntry [entry]
  (let [parts (str/split entry #" ")
        mode (nth parts 0)
        type (nth parts 1)
        extra (str/trim-newline (nth parts 2))
        addy (nth (str/split extra #"\t") 0)
        name (nth (str/split extra #"\t") 1)]
    [:li [:tt (str mode " " type " ") [:a {:href (str "/" type "/" addy)} (str addy)] (str " " name)]]))

(defn format-entries [header+entries-byte]
  (let [entries-byte (vec (rest header+entries-byte))
        entries-sep (vec (partition 2 entries-byte))
        headers (as-> entries-sep $
                  (map first $)
                  (map vec $)
                  (map hashing/bytes->str $))
        modes (as-> headers $
                (map first (map #(str/split % #" ") $))
                (map #(str/replace % #"40000" "040000 tree") $)
                (map #(str/replace % #"100644" "100644 blob") $))
        names (map second (map #(str/split % #" ") headers))
        addresses (as-> entries-sep $
                    (map second $)
                    (map vec $)
                    (map hashing/to-hex-string $))
        entry-format "%s %s\t%s\n"
        entries-tot (map (fn [modes addresses names]
                           (format entry-format modes addresses names)) modes addresses names)]
    (map #(treeEntry %) entries-tot)))

(defn commitFound [addy]
  {:status 302
   :headers {"Location" (str "/commit/" addy)}})

(defn treeBody [dir db addy addy_full]
  ;;entries is what is passed into format-entries function
  (let [entries (commit-tree/get-entries dir db addy_full)]
    (eval (html5 [:head [:title "ResponderHeader"]]
                 [:body
                  [:h1 (str "Tree " addy)]
                  [:ul {:class "tree-entries"} (format-entries entries)]]))))

(defn treeFound [dir db addy addy_full]
  {:status  200
   :headers {"Content-type" "text/html"}
   :body (treeBody dir db addy addy_full)})

(defn treeEndpoint [dir db addy]
  (let [addy_handler (hashing/get-address-endpoint {:addr addy :db db :dir dir})
        addy_coll (:addr addy_handler)
        is_one (:one addy_handler)]
    ;;the address used NEEDS to be the one returned form addy_handler so that it is the full address, not addy
    (if is_one
      (let [filepath (hashing/address-conv dir db addy_coll)
            addy_full addy_coll]
        (if (.exists (io/file filepath))
          (let  [object-type (first (str/split (apply str (map hashing/bytes->str (hashing/split-at-byte 0 (hashing/unzip (hashing/address-conv dir db addy_full))))) #" "))]
            (cond
              (= (str object-type) "commit") (commitFound addy)
              (= (str object-type) "blob") (endpoint_commit/blobFound addy)
              :else (treeFound dir db addy addy_full)))
          {:status 404}))
      (endpoint_commit/multipleCommits addy_coll dir db (take 2 addy)))))