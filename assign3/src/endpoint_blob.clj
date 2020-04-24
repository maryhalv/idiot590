(ns endpoint_blob
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [hashing])
  (:require [hiccup.page :refer [html5]])
  (:require [endpoint_commit]))

(defn commitFound [addy]
  {:status 302
   :headers {"Location" (str "/commit/" addy)}})

(defn treeFound [addy]
  {:status 302
   :headers {"Location" (str "/tree/" addy)}})

(defn blobContent [dir db address]
  (let [object (hashing/bytes->str (second (hashing/split-at-byte 0 (hashing/unzip (hashing/address-conv dir db address)))))]
    object))

(defn blobFound [dir db addy addy_full]
  (eval (html5 [:head [:title "ResponderHeader"]]
               [:body
                [:h1 (str "Blob " addy)]
                [:pre (str (blobContent dir db addy_full))]])))

(defn blobResponse [dir db addy addy_full]
  {:status  200
   :headers {"Content-type" "text/html"}
   :body (blobFound dir db addy addy_full)})

(defn blobEndpoint [dir db addy]
  (let [addy_handler (hashing/get-address-endpoint {:addr addy :db db :dir dir})
        addy_coll (:addr addy_handler)
        is_one (:one addy_handler)]
    (if is_one
      (let [filepath (hashing/address-conv dir db addy_coll)
            addy_full addy_coll]
        (if (.exists (io/file filepath))
          (let  [object-type (first (str/split (apply str (map hashing/bytes->str (hashing/split-at-byte 0 (hashing/unzip (hashing/address-conv dir db addy_full))))) #" "))]
            (cond
              (= (str object-type) "commit") (commitFound addy)
              (= (str object-type) "tree") (treeFound addy)
              :else (blobResponse dir db addy addy_full)))
          {:status 404}))
      (endpoint_commit/multipleCommits addy_coll dir db (take 2 addy)))))