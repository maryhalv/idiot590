(ns log
  (:require [clojure.java.io :as io])
  (:import (java.io File))
  (:require revList))

(defn logHelp []
  (println "idiot log: print abbreviated commit addresses and commit summaries\n")
  (println "Usage: idiot log --oneline [-n <count>] [<ref>]\n")
  (println "Arguments:")
  (println "   -n <count>   stop after <count> revisions (default: don't stop)")
  (println "   <ref>        a reference; see the rev-parse command (default: HEAD)"))

(defn log [{:keys [arg dir db]}]
  ;;need to splice the --oneline command from arg for passing to list-commits methods by passing in (rest arg)
  (cond
    (or (= (first arg) "-h") (= (first arg) "--help")) (logHelp)
    (not (.exists (io/file (str dir File/separator db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
    (or (not (= (first arg) "--oneline")) (nil? (first arg)))  (println "Error: log requires the --oneline switch")
    (and (= (second arg) "-n") (nil? (nthnext arg 2))) (println "Error: you must specify a numeric count with '-n'.")
    (and (= (second arg) "-n") (not (number? (read-string (nth arg 2))))) (println "Error: the argument for '-n' must be a non-negative integer.")
    (and (= (second arg) "-n") (< (Integer/parseInt (nth arg 2)) 0)) (println "Error: the argument for '-n' must be a non-negative integer.")
    (and (= (second arg) "-n") (nil? (nthnext arg 3))) (revList/refHead dir db true (Integer/parseInt (nth arg 2)))
    (and (= (second arg) "-n") (revList/refCheck (rest arg) dir db true)) (revList/list-commits-count (rest arg) dir db true)
    (and (= (second arg) "-n") (not (revList/refCheck (rest arg) dir db true))) (println (format "Error: could not find ref named %s." (nth arg 3)))
    (and (not (= (second arg) "-n")) (nil? (nthnext arg 1))) (revList/refHead dir db true -1)
    (and (not (= (second arg) "-n")) (not (revList/refCheck (rest arg) dir db false))) (println (format "Error: could not find ref named %s." (second arg)))
    (and (not (= (second arg) "-n")) (revList/refCheck (rest arg) dir db false)) (revList/list-commits (rest arg) dir db true -1)))
