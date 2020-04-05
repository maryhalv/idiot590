(ns commit-tree
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require hashing)
  (:require commit)
  (:import (java.io File)))

(declare commit-tree-er)

(defn commit-tree-er []
  (println "idiot commit-tree: write a commit object based on the given tree\n")
  (println "Usage: idiot commit-tree <tree> -m \"message\" [(-p parent)...]\n")
  (println "Arguments:")
  (println "   -h               print this message")
  (println "   <tree>           the address of the tree object to commit")
  (println "   -m \"<message>\"   the commit message")
  (println "   -p <parent>      the address of a parent commit"))

(defn format-entries [header+entries-byte]
  (let [entries-byte (vec (rest header+entries-byte))
        entries-sep (vec (partition 2 entries-byte))
        headers (as-> entries-sep $
                  (map first $)
                  (map vec $)
                  (map hashing/bytes->str $))
        modes (as-> headers $
                (map first (map #(str/split % #" ") $))
                (map #(str/replace % #"40000" "040000 tree") $)
                (map #(str/replace % #"100644" "100644 blob") $))
        names (map second (map #(str/split % #" ") headers))
        addresses (as-> entries-sep $
                    (map second $)
                    (map vec $)
                    (map hashing/to-hex-string $))
        entry-format "%s %s\t%s\n"
        entries-tot (map (fn [modes addresses names]
                           (format entry-format modes addresses names)) modes addresses names)]
    (print (apply str entries-tot))))

(defn get-entries [dir db address]
  (let [bytes (vec (hashing/unzip (hashing/address-conv dir db address)))]
    (loop [entries []
           raw bytes]

      (if (not (some #(= 0 %) raw))
        (conj entries raw)
        (let [[item rest] (hashing/split-at-byte 0 raw)]

          (if (< (count entries) 2)
            (recur (conj entries item)
                   rest)
            (let [addr-header-count (count item)
                  address (take 20 item)
                  header (take-last (- addr-header-count 20) item)]
              (recur (conj entries address header) rest))))))))

(defn format-parent [arg]
  (str "parent " arg "\n"))

(defn commit-object [tree-addr message parent]
  (let [author-str "Linus Torvalds <torvalds@transmeta.com> 1581997446 -0500"
        tree-str tree-addr
        parent-str (apply str (map format-parent (map second parent)))
        commit-format (str "tree %s\n"
                           "author %s\n"
                           "committer %s\n"
                           "\n"
                           "%s\n")
        commit-str (format commit-format
                           tree-str
                           author-str
                           author-str
                           message)
        commit-format-p (str "tree %s\n"
                             "%s"
                             "author %s\n"
                             "committer %s\n"
                             "\n"
                             "%s\n")
        commit-str-p (format commit-format-p
                             tree-str
                             parent-str
                             author-str
                             author-str
                             message)]
    (cond
      (= parent nil) (format "commit %d\000%s"
                             (count commit-str)
                             commit-str)
      :else (format "commit %d\000%s"
                    (count commit-str-p)
                    commit-str-p))))

(defn commit-tree [{:keys [arg dir db com]}]
  (let [[tree-addr m-switch message & parent-raw] arg
        parent (vec (partition 2 parent-raw))

        address-err-check (reduce
                            (fn [errors parent]
                              (cond
                                (> 4 (count (second parent))) (conj errors (format "Error: too few characters specified for address '%s'" (second parent)))
                                :else (let [addr-handler (hashing/get-address {:addr (second parent) :db db :dir dir})]
                                        (cond
                                          (contains? addr-handler :error) (conj errors (:error addr-handler))
                                          :else (identity errors))))) [] parents)
        fixed-addresses (reduce
                          (fn [new-addrs parent]
                            (cond
                              (> 4 (count (second parent))) (identity new-addrs)
                              :else (let [addr-handler (hashing/get-address {:addr (second parent) :db db :dir dir})]
                                      (cond
                                        (contains? addr-handler :error) (identity new-addrs)
                                        :else (conj new-addrs (:addr addr-handler)))))) [] parents)

        vec-not-obj (reduce
                     (fn [bads parent]
                       (if (not (.exists (io/file (hashing/address-conv dir db parent)))) (conj bads parent) (identity bads))) [] fixed-addresses)
        vec-yes-obj (reduce
                     (fn [goods parent]
                       (if (.exists (io/file (hashing/address-conv dir db parent))) (conj goods parent) (identity goods))) [] fixed-addresses)
        vec-non-commits (reduce
                         (fn [non-coms objs]
                           (if (not= "commit" (first (str/split (apply str (map hashing/bytes->str (hashing/split-at-byte 0 (hashing/unzip (hashing/address-conv dir db objs))))) #" ")))
                             (conj non-coms objs) (identity non-coms))) [] vec-yes-obj)
        p-sufficient (not= "-p" (last parent-raw))]

    (cond
      (or (= tree-addr "-h") (= tree-addr "--help")) (if (= com "commit-tree")
                                                       (commit-tree-er)
                                                       (commit/commit-error))
      (not (.exists (io/file (str dir File/separator db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (= nil tree-addr) (println "Error: you must specify a tree address.")
      (= 40 (count tree-addr)) (cond
                                 (not (.exists (io/file (hashing/address-conv dir db tree-addr)))) (println "Error: no tree object exists at that address.")
                                 (not (empty? address-err-check)) (println (first address-err-check))
                                 (not= "tree" (subs (apply str (map hashing/bytes->str (hashing/split-at-byte 0 (hashing/unzip (hashing/address-conv dir db tree-addr))))) 0 4)) (println "Error: an object exists at that address, but it isn't a tree.")
                                 (not= m-switch "-m") (println "Error: you must specify a message.")
                                 (and (= m-switch "-m") (= nil message)) (println "Error: you must specify a message with the -m switch.")
                                 (not= [] vec-not-obj) (println (format "Error: no commit object exists at address %s." (first vec-not-obj)))
                                 (not= [] vec-non-commits) (println (format "Error: an object exists at address %s, but it isn't a commit." (first vec-non-commits)))
                                 (not p-sufficient) (println "Error: you must specify a commit object with the -p switch.")
                                 :else (let [commit-object (commit-object tree-addr message parent)
                                             commit-addr (hashing/to-hex-string (hashing/sha-bytes (.getBytes commit-object)))
                                             commit-path (hashing/address-conv dir db commit-addr)]
                                         (if (= com "commit-tree")
                                           (println commit-addr)
                                           (do
                                             (println "Commit created.\n")
                                             (commit/updateHead {:addr commit-addr :dir dir :db db})))
                                         (io/make-parents commit-path)
                                         (io/copy (hashing/zip-str commit-object) (io/file commit-path))))
      (> 4 (count tree-addr)) (println (format "Error: too few characters specified for address'%s'" tree-addr))
      :else (let [address-handler (hashing/get-address {:addr tree-addr :db db :dir dir})]
              (cond
                (contains? address-handler :error) (println (:error address-handler))
                :else (let [tree-addr (:addr address-handler)]
                        (cond
                          (not (.exists (io/file (hashing/address-conv dir db tree-addr)))) (println "Error: no tree object exists at that address.")
                          (not (empty? address-err-check)) (println (first address-err-check))
                          (not= "tree" (subs (apply str (map hashing/bytes->str (hashing/split-at-byte 0 (hashing/unzip (hashing/address-conv dir db tree-addr))))) 0 4)) (println "Error: an object exists at that address, but it isn't a tree.")
                          (not= m-switch "-m") (println "Error: you must specify a message.")
                          (and (= m-switch "-m") (= nil message)) (println "Error: you must specify a message with the -m switch.")
                          (not= [] vec-not-obj) (println (format "Error: no commit object exists at address %s." (first vec-not-obj)))
                          (not= [] vec-non-commits) (println (format "Error: an object exists at address %s, but it isn't a commit." (first vec-non-commits)))
                          (not p-sufficient) (println "Error: you must specify a commit object with the -p switch.")
                          :else (let [commit-object (commit-object tree-addr message parent)
                                      commit-addr (hashing/to-hex-string (hashing/sha-bytes (.getBytes commit-object)))
                                      commit-path (hashing/address-conv dir db commit-addr)]
                                  (if (= com "commit-tree")
                                    (println commit-addr)
                                    (do
                                      (println "Commit created.\n")
                                      (commit/updateHead {:addr commit-addr :dir dir :db db})))
                                  (io/make-parents commit-path)
                                  (io/copy (hashing/zip-str commit-object) (io/file commit-path))))))))))


