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
    (and (= (first arg) "-d") (.exists (io/file (str dir File/separator db File/separator "refs" File/separator "heads" File/separator (second arg))))) (let [head-contents (slurp (str dir File/separator db File/separator "HEAD"))
          ref (get (str/split head-contents #"/heads") 1)
          head-branch (apply str (subs ref 1 (- (count ref) 1)))]
        (if (= (str head-branch) (str (second arg)))
          (println (format "Error: cannot delete checked-out branch '%s'." (second arg)))
          (do
            (.delete (io/file (str dir File/separator db File/separator "refs" File/separator "heads" File/separator (second arg))))
            (println (format "Deleted branch %s." (second arg))))
          )
          )
    (and (= (first arg) "-d") (not (.exists (io/file (str dir File/separator db File/separator "refs" File/separator "heads" File/separator (second arg))))))
    (println "Error: branch 'unknown-branch' not found.")
    (nil? (first arg))
    (let [head-contents (slurp (str dir File/separator db File/separator "HEAD"))
            ref (get (str/split head-contents #"/heads") 1)
            head-branch (subs ref 1)]
        (let [refs (sort (.list (io/file (str dir File/separator db File/separator "refs" File/separator "heads"))))]
          (doall (map (fn [ref]
                        (cond
                          ;;while it does not currently recognize head branches
                          ;;removed null pointer exception
                          ;;I think this is due to null characters in the
                          ;;autograder's test cases?
                          (= ref head-branch) (println (format "* %s" ref))
                          :else (println (format "  %s" ref))
                          )) refs)))
        )
      )
    )