(ns revList
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:import (java.io File)))

(defn revHelp []
  (println "idiot rev-list: list preceding revisions, latest first\n")
  (println "Usage: idiot rev-list [-n <count>] [<ref>]\n")
  (println "Arguments:")
  (println "   -n <count>   stop after <count> revisions (default: don't stop)")
  (println "   <ref>        a reference; see the rev-parse command (default: HEAD)"))

(def tally -1)

(defn refCheck [arg dir db is-n]
  (if is-n
    (do
      (if (not (nil? (nthnext arg 2)))
        (let [ref (nth arg 2)]
          (cond
            (= ref "@") (.exists (io/file (str dir File/separator db File/separator "HEAD")))
            (= ref "HEAD") (.exists (io/file (str dir File/separator db File/separator "HEAD")))
            :else
            (.exists (io/file (str (str dir File/separator db File/separator "refs" File/separator "heads" File/separator ref))))))))
    (do
      (if (not (nil? (first arg)))
        (let [ref (first arg)]
          (cond
            (= ref "@") (.exists (io/file (str dir File/separator db File/separator "HEAD")))
            (= ref "HEAD") (.exists (io/file (str dir File/separator db File/separator "HEAD")))
            :else
            (.exists (io/file (str (str dir File/separator db File/separator "refs" File/separator "heads" File/separator ref))))))))))

(defn get-commit [dir db addy]
  (let [address (nth (str/split addy #"\n") 0)
        filepath (hashing/address-conv dir db address)
        commitObject (hashing/bytes->str (second (hashing/split-at-byte 0 (hashing/unzip filepath))))]
   (if (> tally 0)
     (println address)
     (do
       (if (= tally -1)
         (println address))))
    (let [commits (str/split commitObject #"\n")
          parent (nth commits 1)
          parent-header (str/split parent #"\s+")
          header (nth parent-header 0)
           pAddy (nth parent-header 1)]
         (if (= header "parent")
           (do
             (def tally (- tally 1))
             (get-commit dir db pAddy))))))

(defn refHead [dir db]
  (let [file (str dir File/separator db File/separator "HEAD")
        headContent (slurp (io/file file))]
    (cond
      (= (apply str (take 4 headContent)) "ref:")
      (do
        (let [addy (slurp (str dir File/separator db File/separator (apply str (butlast (apply str (subs headContent 5))))))]
           (get-commit dir db addy)))
      :else (get-commit dir db headContent))))

(defn refOther [dir db ref]
  (let [file (str dir File/separator db File/separator "refs" File/separator "heads" File/separator ref)
        commitAddy (slurp (io/file file))]
    (get-commit dir db commitAddy)))

(defn list-commits-count [arg dir db]
   (let [count (nth arg 1)
         ref (nth arg 2)]
     (def tally (Integer/parseInt count))
     (if (or (= ref "@") (= ref "HEAD"))
       (refHead dir db)
       (refOther dir db ref))))

(defn list-commits [arg dir db]
  ;;unzip each commit object and check if it has a parent header, and look up the commit at the parent header...etc
  (if (not (nil? (nthnext arg 0)))
    (let [ref (nth arg 0)]
      (if (or (= ref "@") (= ref "HEAD"))
       (refHead dir db)
       (refOther dir db ref)))))

(defn rev-list [{:keys [arg dir db]}]
  (cond
    (or (= (first arg) "-h") (= (first arg) "--help")) (revHelp)
    (and (= (first arg) "-n") (nil? (nthnext arg 1))) (println "Error: you must specify a numeric count with '-n'.")
    (and (= (first arg) "-n") (< (Integer/parseInt (second arg)) 0)) (println "Error: the argument for '-n' must be a non-negative integer.")
    (and (= (first arg) "-n") (refCheck arg dir db true)) (list-commits-count arg dir db)
    (and (= (first arg ) "-n") (not (refCheck arg dir db true))) (println "Error: could not find ref named <ref>.")
    (and (not= (first arg ) "-n") (not (refCheck arg dir db true))) (println "Error: could not find ref named <ref>.")
    (and (not (= (first arg ) "-n")) (refCheck arg dir db false)) (list-commits arg dir db)))


