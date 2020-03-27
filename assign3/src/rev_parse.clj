(ns rev_parse
  (:require [clojure.java.io :as io])
  (:import (java.io File))
  )


(defn rev-parse-er []
      (println "idiot rev-parse: determine which commit a ref points to

Usage: idiot rev-parse <ref>

<ref> can be:
- a branch name, like 'master'
- literally 'HEAD'
- literally '@', an alias for 'HEAD'"))

(defn rev-parse [{:keys [arg dir db]}]

      (cond
        (or (= (first arg) "-h") (= (first arg) "--help")) (rev-parse-er)
        (= arg nil) (println "Error: you must specify a branch name.")
        (> (count arg) 1) (println "Error: you must specify a branch name and nothing else.")
        (not (.exists (io/file (str dir File/separator db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
        :else (let [arg (first arg)]
                   (cond
                     (or (= arg "HEAD") (= arg "@")) (let [arg "HEAD"
                                                           contents (slurp (str dir File/separator db File/separator arg))]
                                                          (cond
                                                            (not= "ref:" (apply str (take 4 contents))) (print contents)
                                                            :else (let [ref (slurp (str dir File/separator db File/separator (apply str (butlast (apply str (subs contents 5))))))]
                                                                       (print ref))))
                     :else (let [ref (str dir File/separator db File/separator "refs" File/separator "heads" File/separator arg)]
                                (cond
                                  (not (.exists (io/file ref))) (println (format "Error: could not find ref named %s." arg))
                                  :else (print (slurp ref))))))))