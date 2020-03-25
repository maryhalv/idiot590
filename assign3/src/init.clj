(ns init
  (:require [clojure.java.io :as io])
  (:import (java.io File))
  )

(defn init-error []
  (println "idiot init: initialize a new database

Usage: idiot init

Arguments:
   -h   print this message"))


(defn read-arg-init [{:keys [arg db dir]}]
  (let [arg-s (first arg)]
    (cond
      (or (= arg-s "-h") (= arg-s "--help")) (init-error)
      (not= nil arg-s) (println "Error: init accepts no arguments")
      (.exists (io/file (str dir File/separator db))) (println "Error: .idiot directory already exists")
      :else (do
              (println "Initialized empty Idiot repository in .idiot directory")
              (.mkdirs (io/file (str dir File/separator db File/separator "objects")))
              (.mkdirs (io/file (str dir File/separator db File/separator "refs" File/separator "heads")))
              ))))
