(ns write-wtree
  (:require [clojure.java.io :as io])
  (:require hashing)
  (:import (java.io File)))

(declare wtree-error, store-entry, ->Entry)

(defn wtree-error []
  (println "idiot write-wtree: write the working tree to the database\n")
  (println "Usage: idiot write-wtree\n")
  (println "Arguments:")
  (println "   -h       print this message"))

(defn make-addr-blob
  "Makes addresses for blobs. Copied code from hash object."
  [contents]
  (hashing/sha-bytes (.getBytes (hashing/header+blob "blob" contents))))

(defrecord FileSystemEntry [type parent-path dir db name contents])

(defn ->FileEntry [parent-path name dir db]
  (let [file (io/file parent-path name)]
    (->FileSystemEntry :file parent-path dir db name (slurp file))))

(declare ->Entry)

(defn ->DirEntry [parent-path name dir db]
  (if (not= nil (seq (.list (io/file parent-path name))))
    (let [file (io/file parent-path name)
          dir-path (str parent-path File/separator name)
          child->entry #(->Entry dir-path % dir db)
          contents (->> file .list sort  (mapv child->entry))]
      (->FileSystemEntry :dir parent-path dir db name contents)) nil))

(defn ->Entry [parent-path name dir db]
  (let [file (io/file parent-path name)]
    (assert (.exists file))
    (if (.isDirectory file)
      (->DirEntry parent-path name dir db)
      (->FileEntry parent-path name dir db))))

(defn remove-subdir [entry subdir-name]
  (letfn [(filter-by-name [entries]
            (filterv #(not= subdir-name (:name %)) entries))]
    (update entry :contents filter-by-name)))

(declare store-entry)

(defn make-header [{:keys [name type addr]}]
  (if (= type :file)
    (hashing/to-hex-string (concat (.getBytes (str "100644 " name "\000")) addr))
    (hashing/to-hex-string (concat (.getBytes (str "40000 " name "\000")) addr))))

(defn store-blob-entry [{:keys [contents dir db]}]
  (let [address (hashing/address-conv dir db (hashing/to-hex-string (make-addr-blob contents)))]
    (if (.exists (io/file address))
      (make-addr-blob contents)
      (do
        (io/make-parents address)
        (io/copy (hashing/zip-str (hashing/header+blob "blob" contents)) (io/file address))
        (make-addr-blob contents)))))

(defn store-tree-entry [{:keys [dir db contents]}]
  (if (and (not= contents nil) (not= contents []) (not= contents [nil]))
    (let [entries+addresses (mapv (juxt identity store-entry) (vec (remove nil? contents)))
          entry->debug-str (fn [[{:keys [name type contents]} addr]] {:name name :type type :addr addr :contents contents})
          entries-byte (as-> entries+addresses $
                         (map entry->debug-str $) ;This returns a map of {:name name :type type :addr addr :contents contents} for each entry
                         (filter #(not= [nil] (:contents %)) $) ; ignore this, trying to figure out how to skip empty dirs
                         (map make-header $) ;Using previous map, I make a string from the "number" (eg 100644) name null char.
                          ;Then I get the bytes and concat with the address (which is already in bytes). Then I turn it to hex-string because you can't make it a reg string
                         (apply str $) ;Here I apply string to join entries together. Idk why it works but it does.
                         (hashing/from-hex-string $)) ;Back to bytes fo sho fo sho
          tree-contents (byte-array (concat (.getBytes (str "tree " (count entries-byte) "\000")) entries-byte)) ;this applies the "header" of "tree (count contents) etc"
          tree-addr (hashing/sha-bytes tree-contents) ;final address
          address (hashing/address-conv dir db (hashing/to-hex-string tree-addr))]
      (if (not (.exists (io/file address))) (do
                                              (io/make-parents address)
                                              (io/copy (hashing/zip-str tree-contents) (io/file address))) nil)

      tree-addr) nil))

(defn store-entry [{:keys [type contents] :as entry}]
  (cond
    (= contents nil) nil
    (= type :file) (store-blob-entry entry)
    :else (store-tree-entry entry)))

(defn write-wtree [{:keys [arg dir db]}]
  (let [arg-s (first arg)]
    (cond
      (or (= arg-s "-h") (= arg-s "--help")) (wtree-error)
      (not (.exists (io/file (str dir File/separator db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (not= arg-s nil) (println "Error: write-wtree accepts no arguments")
      :else (let [address (store-entry (remove-subdir (->Entry "." dir dir db) db))]
              (cond
                (= address nil) (println "The directory was empty, so nothing was saved.")
                :else (println (hashing/to-hex-string address)))))))
