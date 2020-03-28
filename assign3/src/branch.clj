(ns branch
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:import (java.io File))
  )

(defn branch-er []
  (println "idiot branch: list or delete branches

Usage: idiot branch [-d <branch>]

Arguments:
   -d <branch>   delete branch <branch>"))


(defn branch [{:keys [arg dir db]}]
  (cond
    (or (= (first arg) "-h") (= (first arg) "--help")) (branch-er)
    (and (= "-d" (first arg)) (= '() (rest arg))) (println "Error: you must specify a branch name.")
    (not (.exists (io/file (str dir File/separator db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
    (and (not= nil arg) (or (not= "-d" (first arg)) (not= 1 (count (rest arg))))) (println "Error: invalid arguments.")
    :else (let [[switch branch] arg
                branch-path (str dir File/separator db File/separator "refs" File/separator "heads" File/separator branch)]
            (cond
              (and (= "-d" switch) (not (.exists (io/file branch-path)))) (println (format "Error: branch '%s' not found." branch))
              (.startsWith (slurp (str dir File/separator db File/separator "HEAD")) "ref:")
              (let [head-contents (slurp (str dir File/separator db File/separator "HEAD"))
                    head-branch (first (str/split (second (str/split head-contents #"heads/")) #"\n"))
                    ]
                (cond
                  (and (= switch "-d") (= branch head-branch)) (println (format "Error: cannot delete checked-out branch '%s'." branch))
                  (= switch "-d") (do
                                    (.delete (io/file branch-path))
                                    (println (format "Deleted branch %s." branch)))
                  (.startsWith head-contents "ref:") (let [refs (sort (seq (.list (io/file (str dir File/separator db File/separator "refs" File/separator "heads")))))
                                                           head-contents (slurp (str dir File/separator db File/separator "HEAD"))
                                                           head-branch (first (str/split (second (str/split head-contents #"heads/")) #"\n"))]
                                                       (print (apply str (doall (map (fn [ref]
                                                                                       (cond (= ref head-branch)
                                                                                             (str "* " ref "\n") :else (str "  " ref "\n"))) refs)))))
                  :else (println "should not be here1")))
              :else (cond
                      (= switch "-d") (do
                                        (.delete (io/file branch-path))
                                        (println (format "Deleted branch %s." branch)))
                      :else (let [refs (sort (seq (.list (io/file (str dir File/separator db File/separator "refs" File/separator "heads")))))]
                              (print (apply str (map (fn [ref] (str "  " ref "\n")) refs))))
                      :else (println "should not be here2"))))))
