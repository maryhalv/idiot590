(ns hash-object
  (:require [clojure.java.io :as io])
  (:require hashing)
  (:import (java.io File)))

(defn hash-error []
  (println "idiot hash-object: compute address and maybe create blob from file

Usage: idiot hash-object [-w] <file>

Arguments:
   -h       print this message
   -w       write the file to database as a blob object
   <file>   the file"))

(defn read-arg-ho [{:keys [arg dir db]}]
  (cond
    (or (= (first arg) "-h") (= (first arg) "--help")) (hash-error)
    (not (.exists (io/file (str dir File/separator db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
    (and (= (first arg) "-w") (= (first (rest arg)) nil)) (println "Error: you must specify a file.")
    (and (= (first arg) "-w") (not= (rest arg) nil)) (let [file (str dir File/separator (first (rest arg)))]
                                                       (cond
                                                         (not (.exists (io/file file))) (println "Error: that file isn't readable")
                                                         :else (let [address (hashing/sha1-sum (hashing/header+blob "blob" (slurp file)))]
                                                                 (println address)
                                                                 (io/make-parents (hashing/address-conv dir db address))
                                                                 (io/copy (hashing/zip-str (hashing/header+blob "blob" (slurp file))) (io/file (hashing/address-conv dir db address))))))
    (= arg nil) (println "Error: you must specify a file.")
    :else (let [file (str dir File/separator (first arg))]
            ;;hard coded in the type "blob." Will have to change when we use other types
            (cond
              (not (.exists (io/file file))) (println "Error: that file isn't readable")
              :else (let [address (str (hashing/sha1-sum (hashing/header+blob "blob" (slurp file))))]
                      (println address))))))