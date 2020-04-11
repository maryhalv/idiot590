(ns jst-explore-test
  (:require [clj-http.client :as http]
            [clojure.core.async :refer [alts!! go go-loop timeout]]
            [clojure.data.json :as json]
            [clojure.java.shell :refer [sh]]
            [clojure.set :as set]
            [clojure.string :as str])
  (:import (java.io BufferedReader InputStreamReader IOException)
           java.net.ConnectException))

(def global-timeout-secs
  "How long the entire program will wait on a result before giving up."
  20)

(def process-heartbeat-period-msecs
  "How long to sleep between checks of whether the server process is alive."
  50)

(def nonblocking-timeout-msecs
  "How long to wait on a nonblocking result before giving up."
  100)

(def startup-grace-period-msecs
  "How long to allow the process to start before attempting to send an HTTP
  request. This is important because in the event the server can't bind to the
  given port, we _would_ get an HTTP response back from the already-bound
  service, less we gave the process a chance to start and throw an exception
  before we start sending requests."
  (* 5 1000))

(def http-request-period-msecs
  "How long to sleep between attempts to get a response from the server."
  200)

(def process-destroy-grace-msecs
  "How long to wait after nicely destroying the process before forcibly
  destroying the process."
  1000)

(def tmp-dir "jst-explore-tmp-dir")

(defn- setup [tmp-dir]
  (let [{:keys [exit] :as result} (sh "bash" "setup-explore-test.sh" tmp-dir)]
    (assert (zero? exit) (prn-str result))))

(defn- start-server [port]
  (let [cmd-words ["clojure" "-m" "idiot" "-r" tmp-dir "explore" "-p" (str port)]]
    (.start (new ProcessBuilder (into-array cmd-words)))))

(defn- process-result-channel [process]
  (go-loop []
    (if (.isAlive process)
      (do
        (Thread/sleep process-heartbeat-period-msecs)
        (recur))
      (let [stderr (try (slurp (.getErrorStream process))
                        (catch IOException _ ""))
            base {:stderr stderr}
            regex #"\nFull report at:\n([^\n]+)\n"
            report-path (second (re-find regex stderr))]
        (if-not report-path
          base
          (assoc base :full-report (slurp report-path)))))))

(defn- make-one-request [port]
  (let [url (str "http://localhost:" port)]
    (try
      [true (http/get url {:throw-exceptions false})]
      (catch Exception e
        [false e]))))

(defn- response-channel [port]
  (go-loop []
    ;; sleep initially to give a chance for the process to throw an address
    ;; already bound or similar exception.
    (Thread/sleep startup-grace-period-msecs)
    (let [[ok? result] (make-one-request port)]
      (cond
        ok? [true result]
        ;; This is a bit odd b/c we have to recur from tail position only.
        ;; The goal is to poll until the server is up and listening.
        ;; ConnectException means no server is listening on that port.
        (not= ConnectException (type result)) [false result]
        :else (do
                (Thread/sleep http-request-period-msecs)
                (recur))))))

(defn- kill-process [process]
  (println "destroying server process...")
  (.destroy process)
  (Thread/sleep process-destroy-grace-msecs)
  (when (.isAlive process)
    (println "server process still alive; forcibly destroying process...")
    (.destroyForcibly process)))

(defn- get-process-output-line [process]
  (go (->> (.getInputStream process)
           (new InputStreamReader)
           (new BufferedReader)
           .readLine)))

(defn- make-result [message passed? fail-output]
  {:name message
   :score (if passed? 1 0)
   :max_score 1
   :output (if passed? "" fail-output)})

(defn- make-results [raw-results]
  (let [make-msg #(str "The explore subcommand in a repo context " %)
        msg-roots ["starts an HTTP server that responds to requests"
                   "prints a status message about the server starting"
                   "gives a response whose status is 200"
                   "gives a response whose content type is text/html"
                   "gives a response whose body has an HTML5 doctype header"
                   "gives a response whose body is well-formed HTML"
                   "gives a response whose body includes all branch names"]
        messages (map make-msg msg-roots)]
    (mapv (fn [msg [score fail-output]] (make-result msg score fail-output))
          messages raw-results)))

(defn- timeout-results []
  (let [msg (format "The server failed to start within %d seconds"
                    global-timeout-secs)
        result [false msg]
        results (repeat 7 result)]
    (make-results results)))

(defn- process-result-string [{:keys [stderr full-report]}]
  (let [out (str "The standard output from your process has been consumed by the test suite,\n"
                 "which is testing that your output matches the requirements.\n"
                 "If you want to print extra things, e.g. for debugging purposes, wrap your\n"
                 "print statements in a binding block to print to stderr instead, like so:\n"
                 "(binding [*out* *err*] (prn ,,,))\n")
        between-braces "between these triple braces:"
        make-str #(format "The %s is %s {{{\n%s}}}\n" %1 between-braces %2)
        err (make-str "standard error output from your process" stderr)
        more (make-str "full report referenced in the err output" full-report)]
    (str/join "\n" (if full-report [out err more] [out err]))))

(defn- process-results [process-output]
  (let [first-msg (process-result-string process-output)
        msg "The server process crashed; see above."
        messages (cons first-msg (repeat 6 msg))
        raw-results (mapv #(vector false %) messages)]
    (make-results raw-results)))

(defn- error-results [error]
  (let [first-msg (format (str "Sending an HTTP request threw an exception.\n"
                               "The exception data follows.\n%s")
                          (prn-str error))
        msg "Sending an HTTP request failed; see above."
        messages (cons first-msg (repeat 6 msg))
        raw-results (mapv #(vector false %) messages)]
    (make-results raw-results)))

(defn- test-results [response output-line port]
  (make-results
    [;; 1. started an HTTP server--if we get this far, we're good on this point.
     [true ""]

     ;; 2. printed a status message line
     (let [expected (format "Starting server on port %d." port)]
       [(= output-line expected)
        (format (str "The first line of output received from your process"
                     " should have been \"%s\", but was \"%s\".\n")
                expected (or output-line ""))])

     ;; 3. response status == 200
     [(= 200 (:status response))
      (format (str "Expected response status to be 200, but was `%s`.\n"
                   "(Full response map is `%s`.)")
              (pr-str (:status response))
              (pr-str response))]

     ;; 4. response content type is text/html
     (let [headers (:headers response)
           actual (second
                    (first
                      (filter (fn [[name _]]
                                (= "content-type" (-> name .toLowerCase)))
                              headers)))]
       [(= "text/html" actual)
        (format (str "Expected response headers map to include a 'content-type' header"
                     " with value 'text/html', but it didn't.\n"
                     "Actual headers were: `%s`.\n")
                (pr-str headers))])

     ;; 5. response body has an HTML5 doctype header
     (let [body (:body response)]
       [(boolean (re-find #"^<!DOCTYPE html>" (str/triml body)))
        (format (str "Expected body to start with '<!DOCTYPE html>', but it didn't.\n"
                     "Actual body was: `%s`.")
                (pr-str body))])

     ;; 6. response body is well-formed HTML
     (let [body (:body response)
           re (re-pattern (str "(?s)<html.*>.*"
                               "<head.*>.*"
                               "<title.*>.+</title>.*"
                               "</head>.*"
                               "<body.*>.*"
                               "</body>.*"
                               "</html>"))]
       [(boolean (re-find re body))
        (format (str "Expected body to have opening and closing <html>,"
                     " <head>, <title>, and <body> tags, in the correct order,"
                     " but it did not.\n"
                     "Actual body was: `%s`.\n")
                (pr-str body))])

     ;; 7. response body includes all branch names
     (let [body (:body response)
           has-master? (re-find #"master" body)
           has-feature-x? (re-find #"feature-x" body)]
       [(and has-master? has-feature-x?)
        (format (str "Expected body to have a mention of every valid branch, "
                     "but it didn't.\nActual body was: `%s`.\n")
                (pr-str body))])]))

(defn- alts!!-with-timeout
  [timeout-msecs named-channels]
  (let [timeout-channel (timeout timeout-msecs)
        named-channels (assoc named-channels :timeout timeout-channel)
        channel->name (set/map-invert named-channels)
        [val channel] (alts!! (vec (vals named-channels)))
        name (channel->name channel)]
    (when (not= :timeout name)
      [name val])))

(defn- nonblocking-take [channel]
  (let [[name val] (alts!!-with-timeout nonblocking-timeout-msecs {:_ channel})]
    (when name val)))

(defn -main []
  (setup tmp-dir)
  (let [port 3000
        server-proc (start-server port)
        channels {:process (process-result-channel server-proc)
                  :response (response-channel 3000)}
        timeout-msecs (* global-timeout-secs 1000)
        [name val] (alts!!-with-timeout timeout-msecs channels)

        results (cond
                  (nil? name) (timeout-results)
                  (= :process name) (process-results val)
                  (not (first val)) (error-results (second val))
                  :else (let [response (second val)
                              out-channel (get-process-output-line server-proc)
                              server-out-line (nonblocking-take out-channel)]
                          (test-results response server-out-line port)))]

    (println "Results:")
    (json/pprint results)
    (println (format "Leaving example repo in %s directory if you want to try things manually."
                     tmp-dir))
    (kill-process server-proc)
    (shutdown-agents)))
