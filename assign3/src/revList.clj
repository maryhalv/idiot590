(ns revList
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require hashing)
  (:import (java.io File)))

(defn revHelp []
  (println "idiot rev-list: list preceding revisions, latest first\n")
  (println "Usage: idiot rev-list [-n <count>] [<ref>]\n")
  (println "Arguments:")
  (println "   -n <count>   stop after <count> revisions (default: don't stop)")
  (println "   <ref>        a reference; see the rev-parse command (default: HEAD)"))

(defn refCheck [arg dir db is-n]
  (if is-n
    (if (not (nil? (nthnext arg 2)))
      (let [ref (nth arg 2)]
        (cond
          (= ref "@") (.exists (io/file (str dir File/separator db File/separator "HEAD")))
          (= ref "HEAD") (.exists (io/file (str dir File/separator db File/separator "HEAD")))
          :else
          (.exists (io/file (str (str dir File/separator db File/separator "refs" File/separator "heads" File/separator ref)))))) nil)
    (if (not (nil? (first arg)))
      (let [ref (first arg)]
        (cond
          (= ref "@") (.exists (io/file (str dir File/separator db File/separator "HEAD")))
          (= ref "HEAD") (.exists (io/file (str dir File/separator db File/separator "HEAD")))
          :else
          (.exists (io/file (str (str dir File/separator db File/separator "refs" File/separator "heads" File/separator ref)))))) nil)))

(defn get-commit [dir db addy count]
  (let [address (nth (str/split addy #"\n") 0)
        filepath (hashing/address-conv dir db address)
        commitObject (hashing/bytes->str (second (hashing/split-at-byte 0 (hashing/unzip filepath))))
        tally (if (= (type count) String)
                (Integer/parseInt count)
                count)]
    (if (> tally 0)
      (println address)
      (if (= tally -1)
        (println address) nil))
    (let [commits (str/split commitObject #"\n")
          parent (nth commits 1)
          parent-header (str/split parent #"\s+")
          header (nth parent-header 0)
          pAddy (:addr (hashing/get-address {:addr (nth parent-header 1) :dir dir :db db}))]
      (if (= header "parent")
        (if (and (not (= tally 0)) (not (= tally -1)))
          (get-commit dir db pAddy (- tally 1))
          (get-commit dir db pAddy tally)) nil))))

(defn findMessage [commits line1 line2 i]
  ;;if the count of line 1 is 0, then that means the next line contains the commit message
  ;;so return line2
  (if (= (count line1) 0)
    line2
    (findMessage commits line2 (nth commits (+ i 1)) (+ i 1))))

(defn formatLine [commits line4 line5 addy]
  (if (= (count line4) 0)
    (let [message line5
          trimMessage (nth (str/split (nth message 0) #"\n") 0)]
      (println (str/join (take 7 addy)) trimMessage))
    (let [message line4
          trimMessage (nth (str/split message #"\n") 0)]
      ;;if the line starts with author or committer, need to keep traversing
      (if (or (= (nth (str/split message #"\s+") 0) "author") (= (nth (str/split message #"\s+") 0) "committer"))
        (let [message (findMessage commits (nth line5 0) (nth commits 6) 6)
              trimMessage (nth (str/split message #"\n") 0)]
          (println (str/join (take 7 addy)) trimMessage))
        (println (str/join (take 7 addy)) trimMessage)))))

(defn logCommit [dir db addy tally]
  (let [address (nth (str/split addy #"\n") 0)
        filepath (hashing/address-conv dir db address)
        commitObject (hashing/bytes->str (second (hashing/split-at-byte 0 (hashing/unzip filepath))))
        commits (str/split commitObject #"\n")
        line4 (nth commits 4)
        line5 (nthnext commits 5)
        parent (nth commits 1)
        parent-header (str/split parent #"\s+")
        header (nth parent-header 0)
        pAddy (:addr (hashing/get-address {:addr (nth parent-header 1) :dir dir :db db}))
        count (if (= (type tally) String)
                (Integer/parseInt tally)
                tally)]
    (if (> count 0)
      (formatLine commits line4 line5 addy)
      (if (= count -1)
        (formatLine commits line4 line5 addy) nil))
    (if (= header "parent")
      (if (and (not (= count 0)) (not (= count -1)))
        (logCommit dir db pAddy (- count 1))
        (logCommit dir db pAddy count)) nil)))

(defn refHead [dir db log count]
  (let [file (str dir File/separator db File/separator "HEAD")
        headContent (slurp (io/file file))]
    (cond
      (= (apply str (take 4 headContent)) "ref:")
      (let [addy (slurp (str dir File/separator db File/separator (apply str (butlast (apply str (subs headContent 5))))))]
        (if log
          (logCommit dir db addy count)
          (get-commit dir db addy count)))
      :else (if log
              (logCommit dir db headContent count)
              (get-commit dir db headContent count)))))

(defn refOther [dir db ref log count]
  (let [file (str dir File/separator db File/separator "refs" File/separator "heads" File/separator ref)
        commitAddy (slurp (io/file file))]
    (if log
      (logCommit dir db commitAddy count)
      (get-commit dir db commitAddy count))))

;;have list commits method take in a param for operating under log method, should be true or false
(defn list-commits-count [arg dir db log]
  (let [count (nth arg 1)
        ref (nth arg 2)]
    (if (or (= ref "@") (= ref "HEAD"))
      (refHead dir db log count)
      (refOther dir db ref log count))))

(defn list-commits [arg dir db log count]
  ;;unzip each commit object and check if it has a parent header, and look up the commit at the parent header...etc
  (if (not (nil? (nthnext arg 0)))
    (let [ref (nth arg 0)]
      (if (or (= ref "@") (= ref "HEAD"))
        (refHead dir db log count)
        (refOther dir db ref log count))) nil))

(defn rev-list [{:keys [arg dir db]}]
  (cond
    (or (= (first arg) "-h") (= (first arg) "--help")) (revHelp)
    (not (.exists (io/file (str dir File/separator db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
    (and (= (first arg) "-n") (nil? (nthnext arg 1))) (println "Error: you must specify a numeric count with '-n'.")
    (and (= (first arg) "-n") (not (number? (read-string (second arg))))) (println "Error: the argument for '-n' must be a non-negative integer.")
    (and (= (first arg) "-n") (< (Integer/parseInt (second arg)) 0)) (println "Error: the argument for '-n' must be a non-negative integer.")
    (and (= (first arg) "-n") (nil? (nthnext arg 2))) (refHead dir db false (Integer/parseInt (second arg)))
    (and (= (first arg) "-n") (refCheck arg dir db true)) (list-commits-count arg dir db false)
    (and (= (first arg) "-n") (not (refCheck arg dir db true))) (println (format "Error: could not find ref named %s." (nth arg 2)))
    (and (not (= (first arg) "-n")) (nil? (nthnext arg 0))) (refHead dir db false -1)
    (and (not (= (first arg) "-n")) (not (refCheck arg dir db false))) (println (format "Error: could not find ref named %s." (first arg)))
    (and (not (= (first arg) "-n")) (refCheck arg dir db false)) (list-commits arg dir db false -1)))


