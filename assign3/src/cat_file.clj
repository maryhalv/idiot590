(ns cat-file
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require hashing)
  (:require commit-tree)
  (:import  (java.io File)))

(defn cat-error []
  (println "idiot cat-file: print information about an object\n")
  (println "Usage: idiot cat-file {-p|-t} <address>\n")
  (println "Arguments:")
  (println "   -h          print this message")
  (println "   -p          pretty-print contents based on object type")
  (println "   -t          print the type of the given object")
  (println "   <address>   the SHA1-based address of the object"))

(defn read-arg-cf [{:keys [arg db dir]}]

  (let [arg-s (first arg)
        address (second arg)
        address-size (count address)]
    (cond
      (or (= arg-s "-h") (= arg-s "--help")) (cat-error)
      (not (.exists (io/file (str dir File/separator db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (and (not= arg-s "-t") (not= arg-s "-p")) (println "Error: the -p or -t switch is required")
      (= address nil) (println "Error: you must specify an address")
      (= 40 address-size) (cond
                            (not (.exists (io/file (hashing/address-conv dir db address)))) (println "Error: that address doesn't exist")
                            :else (let [object (hashing/bytes->str (second (hashing/split-at-byte 0 (hashing/unzip (hashing/address-conv dir db address)))))
                                        object-type (first (str/split (apply str (map hashing/bytes->str (hashing/split-at-byte 0 (hashing/unzip (hashing/address-conv dir db address))))) #" "))]
                                    (cond
                                      (= arg-s "-t") (println object-type)
                                      (= "blob" object-type) (print object)
                                      (= "tree" object-type) (commit-tree/format-entries (commit-tree/get-entries dir db address))
                                      (= "commit" object-type) (print object)
                                      :else (prn "shouldn't be here"))))

      (> 4 address-size) (println (format "Error: too few characters specified for address '%s'" address))
      :else (let [address-handler (hashing/get-address {:addr address :db db :dir dir})]
              (cond
                (contains? address-handler :error) (println (:error address-handler))
                :else (let [address (:addr address-handler)]
                        (cond
                          (not (.exists (io/file (hashing/address-conv dir db address)))) (println "Error: that address doesn't exist")
                          :else (let [object (hashing/bytes->str (second (hashing/split-at-byte 0 (hashing/unzip (hashing/address-conv dir db address)))))
                                      object-type (first (str/split (apply str (map hashing/bytes->str (hashing/split-at-byte 0 (hashing/unzip (hashing/address-conv dir db address))))) #" "))]
                                  (cond
                                    (= arg-s "-t") (println object-type)
                                    (= "blob" object-type) (print object)
                                    (= "tree" object-type) (commit-tree/format-entries (commit-tree/get-entries dir db address))
                                    (= "commit" object-type) (print object)
                                    :else (prn "shouldn't be here"))))))))))