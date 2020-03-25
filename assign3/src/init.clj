(ns init
  (:require [clojure.java.io :as io])
  (:import (java.io ByteArrayOutputStream ByteArrayInputStream File)
           (java.util.zip DeflaterOutputStream)
           )
  )

(defn init-error []
  (println "idiot init: initialize a new database

Usage: idiot init

Arguments:
   -h   print this message"))

(defn zip-str
  "Zip the given data with zlib. Return a ByteArrayInputStream of the zipped
  content."
  [data]
  (let [out (ByteArrayOutputStream.)
        zipper (DeflaterOutputStream. out)]
    (io/copy data zipper)
    (.close zipper)
    (ByteArrayInputStream. (.toByteArray out))))


(defn read-arg-init [{:keys [arg db dir]}]
  (let [arg-s (first arg)]
    (cond
      (or (= arg-s "-h") (= arg-s "--help")) (init-error)
      (not= nil arg-s) (println "Error: init accepts no arguments")
      (.exists (io/file (str dir File/separator db))) (println "Error: .idiot directory already exists")
      :else (do
              (println "Initialized empty Idiot repository in .idiot directory")
              (.mkdirs (io/file (str dir File/separator db File/separator "objects")))
              ;;is it okay to create this in one .mkdirs statement?
              (.mkdirs (io/file (str dir File/separator db File/separator "refs" File/separator "heads")))
              ;;(io/copy (zip-str "ref: refs/heads/master\n") (io/file (str dir File/separator db File/separator "refs" File/separator "heads")))
              ))))
