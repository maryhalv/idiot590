(ns idiot
  ;(require '[clojure.java.io :as io])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require init)
  (:import java.security.MessageDigest
           (java.io ByteArrayOutputStream ByteArrayInputStream File)
           (java.util.zip DeflaterOutputStream InflaterInputStream)
    ;(java.io.File)
           ))

(defn top-level-error []
  (println "idiot: the other stupid content tracker

Usage: idiot [<top-args>] <command> [<args>]

Top-level arguments:
   -r <dir>   run from the given directory instead of the current one
   -d <dir>   store the database in <dir> (default: .idiot)

Commands:
   help
   init
   hash-object [-w] <file>
   cat-file {-p|-t} <address>
   write-wtree
   commit-tree <tree> -m \"<message>\" [(-p <parent>)...]"))

(defn help-error []
  (println "idiot help: print help for a command

Usage: idiot help <command>\n\nArguments:\n   <command>   the command to print help for\n\nCommands:\n   help\n   init\n   hash-object [-w] <file>
   cat-file {-p|-t} <address>\n   write-wtree\n   commit-tree <tree> -m \"<message>\" [(-p <parent>)...]"))

;(defn init-error []
;  (println "idiot init: initialize a new database
;
;Usage: idiot init
;
;Arguments:
;   -h   print this message"))

(defn hash-error []
  (println "idiot hash-object: compute address and maybe create blob from file

Usage: idiot hash-object [-w] <file>

Arguments:
   -h       print this message
   -w       write the file to database as a blob object
   <file>   the file"))

(defn cat-error []
  (println "idiot cat-file: print information about an object

Usage: idiot cat-file {-p|-t} <address>

Arguments:
   -h          print this message
   -p          pretty-print contents based on object type
   -t          print the type of the given object
   <address>   the SHA1-based address of the object"))

(declare wtree-error, commit-tree-er)
(defn read-arg-help [{:keys [arg]}]
  ;;changing arg type from sequence to string

  (let [arg-s (first arg)]
    (cond
      (= arg-s nil) (top-level-error)
      (= arg-s "help") (help-error)
      (= arg-s "init") (init/init-error)
      (= arg-s "hash-object") (hash-error)
      (= arg-s "cat-file") (cat-error)
      (= arg-s "write-wtree") (wtree-error)
      (= arg-s "commit-tree") (commit-tree-er)
      (or (= arg-s "-h") (= arg-s "--help")) (help-error)
      :else (println "Error: invalid command"))))

;(defn read-arg-init [{:keys [arg db dir]}]
;  (let [arg-s (first arg)]
;    (cond
;      (or (= arg-s "-h") (= arg-s "--help")) (init-error)
;      (not= nil arg-s) (println "Error: init accepts no arguments")
;      (.exists (io/file (str dir File/separator db))) (println "Error: .idiot directory already exists")
;      :else (do
;              (println "Initialized empty Idiot repository in .idiot directory")
;              (.mkdirs (io/file (str dir File/separator db File/separator "objects")))))))

(defn sha1-hash-bytes [data]
  (.digest (MessageDigest/getInstance "sha1")
           (.getBytes data)))

(defn byte->hex-digits [byte]
  (format "%02x"
          (bit-and 0xff byte)))

(defn bytes->hex-string [bytes]
  (->> bytes
       (map byte->hex-digits)
       (apply str)))

(defn sha1-sum [header+blob]
  (bytes->hex-string (sha1-hash-bytes header+blob)))

(defn header+blob [type blob]
  (let [length (count (str blob))]
    (str type " " length "\000" blob)))

(defn zip-str
  "Zip the given data with zlib. Return a ByteArrayInputStream of the zipped
  content."
  [data]
  (let [out (ByteArrayOutputStream.)
        zipper (DeflaterOutputStream. out)]
    (io/copy data zipper)
    (.close zipper)
    (ByteArrayInputStream. (.toByteArray out))))

(defn address-conv [dir db address]
  (let [[l1 l2 & backend] address]
    (str dir File/separator db File/separator "objects/" l1 l2 "/" (apply str backend))))

(defn read-arg-ho [{:keys [arg dir db]}]
  (cond
    (or (= (first arg) "-h") (= (first arg) "--help")) (hash-error)
    (not (.exists (io/file (str dir File/separator db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
    (and (= (first arg) "-w") (= (first (rest arg)) nil)) (println "Error: you must specify a file.")
    (and (= (first arg) "-w") (not= (rest arg) nil)) (let [file (str dir File/separator (first (rest arg)))]
                                                       (cond
                                                         (not (.exists (io/file file))) (println "Error: that file isn't readable")
                                                         :else (let [address (sha1-sum (header+blob "blob" (slurp file)))]
                                                                 (println address)
                                                                 (io/make-parents (address-conv dir db address))
                                                                 (io/copy (zip-str (header+blob "blob" (slurp file))) (io/file (address-conv dir db address))))))
    (= arg nil) (println "Error: you must specify a file.")
    :else (let [file (str dir File/separator (first arg))]
            ;;hard coded in the type "blob." Will have to change when we use other types
            (cond
              (not (.exists (io/file file))) (println "Error: that file isn't readable")
              :else (let [address (str (sha1-sum (header+blob "blob" (slurp file))))]
                      (println address))))))
(declare unzip)

(defn first2 [arg]
  (let [[l1 l2] arg]
    (str ".git/objects/" l1 l2)))

(defn curate-blob [arg]
  (cond
    (>= 16 (count arg)) (print (subs arg 7))
    (>= 95 (count arg)) (print (subs arg 8))
    :else (print "too large, correct this")))

(declare bytes->str, split-at-byte, to-hex-string, hex-digits->byte)

(defn get-entries [dir db address]
  (let [bytes (vec (unzip (address-conv dir db address)))]
    (loop [entries []
           raw bytes]

      (if (not (some #(= 0 %) raw))
        (conj entries raw)
        (let [[item rest] (split-at-byte 0 raw)]

          (if (< (count entries) 2)
            (recur (conj entries item)
                   rest)
            (let [addr-header-count (count item)
                  address (take 20 item)
                  header (take-last (- addr-header-count 20) item)]
              (recur (conj entries address header) rest))
            ))))))

(defn format-entries [header+entries-byte]
  (let [entries-byte (vec (rest header+entries-byte))
        entries-sep (vec (partition 2 entries-byte))
        headers (as-> entries-sep $
                  (map first $)
                  (map vec $)
                  (map bytes->str $))
        modes (as-> headers $
                (map first (map #(str/split % #" ") $))
                (map #(str/replace % #"40000" "040000 tree") $)
                (map #(str/replace % #"100644" "100644 blob") $))
        names (map second (map #(str/split % #" ") headers))
        addresses (as-> entries-sep $
                    (map second $)
                    (map vec $)
                    (map to-hex-string $))
        entry-format "%s %s\t%s\n"
        entries-tot (map (fn [modes addresses names]
                           (format entry-format modes addresses names)) modes addresses names)]
    (print (apply str entries-tot))))

(defn read-arg-cf [{:keys [arg db dir]}]

  (let [arg-s (first arg) address (second arg)]
    (cond
      (or (= arg-s "-h") (= arg-s "--help")) (cat-error)
      (not (.exists (io/file (str dir File/separator db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (and (not= arg-s "-t") (not= arg-s "-p")) (println "Error: the -p or -t switch is required")
      (= address nil) (println "Error: you must specify an address")
      (not (.exists (io/file (address-conv dir db address)))) (println "Error: that address doesn't exist")
      :else (let [object (bytes->str (second (split-at-byte 0 (unzip (address-conv dir db address)))))
                  object-type (first (str/split (apply str (map bytes->str (split-at-byte 0 (unzip (address-conv dir db address))))) #" "))]
              (cond
                (= arg-s "-t") (println object-type)
                (= "blob" object-type) (print object)
                (= "tree" object-type) (format-entries (get-entries dir db address))
                (= "commit" object-type) (print object)
                :else (prn "shouldn't be here"))))))

(defn wtree-error []
  (println "idiot write-wtree: write the working tree to the database

Usage: idiot write-wtree

Arguments:
   -h       print this message"))

(defn commit-tree-er []
  (println "idiot commit-tree: write a commit object based on the given tree

Usage: idiot commit-tree <tree> -m \"message\" [(-p parent)...]

Arguments:
   -h               print this message
   <tree>           the address of the tree object to commit
   -m \"<message>\"   the commit message
   -p <parent>      the address of a parent commit"))


;;boiler plate code provided


(defn sha-bytes [bytes]
  (.digest (MessageDigest/getInstance "sha1") bytes))

(defn to-hex-string
  "Convert the given byte array into a hex string, 2 characters per byte."
  [bytes]
  (letfn [(to-hex [byte]
            (format "%02x" (bit-and 0xff byte)))]
    (->> bytes (map to-hex) (apply str))))

(def blob-contents "file contents\n")
(def blob-addr (sha-bytes (.getBytes (str "blob 14\000" blob-contents))))

(def tree-contents
  (byte-array (concat (.getBytes "tree 32\000100644 file\000")
                      blob-addr)))

(def tree-addr (sha-bytes tree-contents))

(defn hex-digits->byte
  [[dig1 dig2]]
  ;; This is tricky because something like "ab" is "out of range" for a
  ;; Byte, because Bytes are signed and can only be between -128 and 127
  ;; (inclusive). So we have to temporarily use an int to give us the room
  ;; we need, then adjust the value if needed to get it in the range for a
  ;; byte, and finally cast to a byte.
  (let [i (Integer/parseInt (str dig1 dig2) 16)
        byte-ready-int (if (< Byte/MAX_VALUE i)
                         (byte (- i 256))
                         i)]
    (byte byte-ready-int)))

(defn from-hex-string
  [hex-str]
  (byte-array (map hex-digits->byte (partition 2 hex-str))))

(defn unzip
  "Unzip the given file's contents with zlib."
  [path]
  (with-open [input (-> path io/file io/input-stream)
              unzipper (InflaterInputStream. input)
              out (ByteArrayOutputStream.)]
    (io/copy unzipper out)
    (.toByteArray out)))

;; Note that if given binary data this will fail with an error message
;; like:
;; Execution error (IllegalArgumentException) at ,,,.
;; Value out of range for char: -48
(defn bytes->str [bytes]
  (try
    (->> bytes (map char) (apply str))
    (catch Exception e  (str "caught exception: " (.getMessage e)))))

(defn split-at-byte [b bytes]
  (let [part1 (take-while (partial not= b) bytes)
        part2 (nthrest bytes (-> part1 count inc))]
    [part1 part2]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-addr-blob
  "Makes addresses for blobs. Copied code from hash object."
  [contents]
  (sha-bytes (.getBytes (header+blob "blob" contents))))

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
    (to-hex-string (concat (.getBytes (str "100644 " name "\000")) addr))
    (to-hex-string (concat (.getBytes (str "40000 " name "\000")) addr))))

(defn store-blob-entry [{:keys [contents dir db]}]
  (let [address (address-conv dir db (to-hex-string (make-addr-blob contents)))]
    (if (.exists (io/file address)) (make-addr-blob contents)
        (do
          (io/make-parents address)
          (io/copy (zip-str (header+blob "blob" contents)) (io/file address))
          (make-addr-blob contents)))))

(defn store-tree-entry [{:keys [dir db contents]}]
  (if (and (not= contents nil) (not= contents []) (not= contents [nil]))
    (let [entries+addresses (mapv (juxt identity store-entry) (vec (remove nil? contents)))
          entry->debug-str (fn [[{:keys [name type contents]} addr]] {:name name :type type :addr addr :contents contents})
          entries-byte (as-> entries+addresses $ ;
                         (map entry->debug-str $) ;This returns a map of {:name name :type type :addr addr :contents contents} for each entry
                         (filter #(not= [nil] (:contents %)) $) ; ignore this, trying to figure out how to skip empty dirs
                         (map make-header $) ;Using previous map, I make a string from the "number" (eg 100644) name null char.
                                                  ;Then I get the bytes and concat with the address (which is already in bytes). Then I turn it to hex-string because you can't make it a reg string
                         (apply str $) ;Here I apply string to join entries together. Idk why it works but it does.
                         (from-hex-string $)) ;Back to bytes fo sho fo sho
          tree-contents (byte-array (concat (.getBytes (str "tree " (count entries-byte) "\000")) entries-byte)) ;this applies the "header" of "tree (count contents) etc"
          tree-addr (sha-bytes tree-contents) ;final address
          address (address-conv dir db (to-hex-string tree-addr))]
      (if (not (.exists (io/file address))) (do
                                              (io/make-parents address)
                                              (io/copy (zip-str tree-contents) (io/file address))) nil)

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
                :else (println (to-hex-string address)))))))

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

(defn commit-tree [{:keys [arg dir db]}]
  (let [[tree-addr m-switch message & parent-raw] arg
        parent (vec (partition 2 parent-raw))
        vec-not-obj (reduce
                     (fn [bads parent]
                       (if (not (.exists (io/file (address-conv dir db (second parent))))) (conj bads (second parent)) (identity bads))) [] parent)
        vec-yes-obj (reduce
                     (fn [goods parent]
                       (if (.exists (io/file (address-conv dir db (second parent)))) (conj goods (second parent)) (identity goods))) [] parent)
        vec-non-commits (reduce
                         (fn [non-coms objs]
                           (if (not= "commit" (first (str/split (apply str (map bytes->str (split-at-byte 0 (unzip (address-conv dir db objs))))) #" ")))
                             (conj non-coms objs) (identity non-coms))) [] vec-yes-obj)
        p-sufficient (not= "-p" (last parent-raw))]

    (cond
      (or (= tree-addr "-h") (= tree-addr "--help")) (commit-tree-er)
      (not (.exists (io/file (str dir File/separator db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (= nil tree-addr) (println "Error: you must specify a tree address.")
      (not (.exists (io/file (address-conv dir db tree-addr)))) (println "Error: no tree object exists at that address.")
      (not= "tree" (subs (apply str (map bytes->str (split-at-byte 0 (unzip (address-conv dir db tree-addr))))) 0 4)) (println "Error: an object exists at that address, but it isn't a tree.")
      (not= m-switch "-m") (println "Error: you must specify a message.")
      (and (= m-switch "-m") (= nil message)) (println "Error: you must specify a message with the -m switch.")
      (not= [] vec-not-obj) (println (format "Error: no commit object exists at address %s." (first vec-not-obj)))
      (not= [] vec-non-commits) (println (format "Error: an object exists at address %s, but it isn't a commit." (first vec-non-commits)))
      (not p-sufficient) (println "Error: you must specify a commit object with the -p switch.")
      :else (let [commit-object (commit-object tree-addr message parent)
                  commit-addr (to-hex-string (sha-bytes (.getBytes commit-object)))
                  commit-path (address-conv dir db commit-addr)]
              (println commit-addr)
              (io/make-parents commit-path)
              (io/copy (zip-str commit-object) (io/file commit-path))))))

(defn run-com [{:keys [com dir db arg]}]
  (cond
    (= com "help") (read-arg-help {:arg arg})
    (= com "init") (init/read-arg-init {:arg arg :dir dir :db db})
    (= com "hash-object") (read-arg-ho {:arg arg :dir dir :db db})
    (= com "cat-file") (read-arg-cf {:arg arg :dir dir :db db})
    (= com "write-wtree") (write-wtree {:arg arg :dir dir :db db})
    (= com "commit-tree") (commit-tree {:arg arg :dir dir :db db})
    (or (= com nil) (= com "-h") (= com "--help")) (top-level-error)
    :else (println "Error: invalid command")))

(defn -main [& args]                                        ;;com = command
  ;;separating command from arg

  (let [[flag1 & rest] args]
    (cond
      (= flag1 "-r") (let [[dir & f2dca] rest]
                       (cond
                         (= dir nil) (println "Error: the -r switch needs an argument")
                         (not (.exists (io/file dir))) (println "Error: the directory specified by -r does not exist")
                         :else (let [[flag2 & dca] f2dca]
                                 (cond
                                   (= flag2 "-d") (let [[db & ca] dca]
                                                    (cond
                                                      (= nil db) (println "Error: the -d switch needs an argument")
                                                      :else      (let [[com & arg] ca]
                                                                   (run-com {:com com :dir dir :db db :arg arg}))))
                                   :else (let [[com & arg] f2dca]
                                           (run-com {:com com :dir dir :db ".idiot" :arg arg}))))))
      (= flag1 "-d") (let [[db & f2rca] rest]
                       (cond
                         (= db nil) (println "Error: the -d switch needs an argument")
                         :else (let [[flag2 & rca] f2rca]
                                 (cond
                                   (= flag2 "-r") (let [[dir & ca] rca]
                                                    (cond
                                                      (= dir nil) (println "Error: the -r switch needs an argument")
                                                      (not (.exists (io/file dir))) (println "Error: the directory specified by -r does not exist")
                                                      :else   (let [[com & arg] ca]
                                                                (run-com {:com com :dir dir :db db :arg arg}))))
                                   :else (let [[com & arg] f2rca]
                                           (run-com {:com com :dir "." :db db :arg arg}))))))
      :else (let [[com & arg] args]
              (run-com {:com com :arg arg :dir "." :db ".idiot"})))))