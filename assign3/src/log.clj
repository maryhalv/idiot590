(ns log
  (:require revList)
  )

(defn logHelp []
  (println "idiot log: print abbreviated commit addresses and commit summaries\n")
  (println "Usage: idiot log --oneline [-n <count>] [<ref>]\n")
  (println "Arguments:")
  (println "   -n <count>   stop after <count> revisions (default: don't stop)")
  (println "   <ref>        a reference; see the rev-parse command (default: HEAD)"))

(defn log [{:keys [arg dir db]}]

  )
