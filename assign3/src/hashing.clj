(ns hashing
  (:require [clojure.java.io :as io])
  (:import java.security.MessageDigest
           (java.io ByteArrayOutputStream ByteArrayInputStream File)
           (java.util.zip DeflaterOutputStream InflaterInputStream))
  (:require hashing))

(declare bytes->str, split-at-byte, to-hex-string, hex-digits->byte, unzip)

(defn address-conv [dir db address]
  (let [[l1 l2 & backend] address]
    (str dir File/separator db File/separator "objects/" l1 l2 "/" (apply str backend))))

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

(defn first2 [arg]
  (let [[l1 l2] arg]
    (str ".git/objects/" l1 l2)))

(defn curate-blob [arg]
  (cond
    (>= 16 (count arg)) (print (subs arg 7))
    (>= 95 (count arg)) (print (subs arg 8))
    :else (print "too large, correct this")))

(defn get-address [{:keys [addr db dir]}]
  (let [first2 (apply str (take 2 addr))
        addr-count (count addr)
        last-part (apply str (subs addr 2))
        lp-count (- addr-count 2)
        path (str dir File/separator db File/separator "objects" File/separator first2)
        first-two-exists? (.exists (io/file path))]
    (cond
      first-two-exists? (let [sub-addrs (seq (.list (io/file path)))
                              matching-addrs (reduce (fn [matching-addr addresses]
                                                       (let [addr-cut (apply str (take lp-count addresses))]
                                                         (if (= last-part addr-cut) (conj matching-addr addresses) (identity matching-addr)))) [] sub-addrs)]
                          (cond
                            (< 1 (count matching-addrs)) {:error (format "Error: ambiguous match for address '%s'" addr)}
                            (> 1 (count matching-addrs)) {:error "Error: that address doesn't exist"}
                            :else {:addr (str first2 (first matching-addrs))}))
      :else {:addr (str first2 last-part)})))

(defn get-address-endpoint [{:keys [addr db dir]}]
  (let [first2 (apply str (take 2 addr))
        addr-count (count addr)
        last-part (apply str (subs addr 2))
        lp-count (- addr-count 2)
        path (str dir File/separator db File/separator "objects" File/separator first2)
        first-two-exists? (.exists (io/file path))]
    (cond
      first-two-exists? (let [sub-addrs (seq (.list (io/file path)))
                              matching-addrs (reduce (fn [matching-addr addresses]
                                                       (let [addr-cut (apply str (take lp-count addresses))]
                                                         (if (= last-part addr-cut) (conj matching-addr addresses) (identity matching-addr)))) [] sub-addrs)]
                          (cond
                            (< 1 (count matching-addrs)) {:addr matching-addrs :one false}
                            (> 1 (count matching-addrs)) {:error "Error: that address doesn't exist"}
                            :else {:addr (str first2 (first matching-addrs)) :one true}))
      :else {:addr (str first2 last-part) :one true})))
