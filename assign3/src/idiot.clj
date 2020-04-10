(ns idiot
  (:require [clojure.java.io :as io])
  (:require init)
  (:require cat-file)
  (:require write-wtree)
  (:require commit-tree)
  (:require hashing)
  (:require hash-object)
  (:require switch)
  (:require rev_parse)
  (:require commit)
  (:require branch)
  (:require revList)
  (:require commitTree2)
  (:require writeTree2)
  (:require log))

(defn top-level-error []
  (println "idiot: the other stupid content tracker\n")
  (println "Usage: idiot [<top-args>] <command> [<args>]\n")
  (println "Top-level arguments:")
  (println "   -r <dir>   run from the given directory instead of the current one")
  (println "   -d <dir>   store the database in <dir> (default: .idiot)\n")
  (println "Commands:")
  (println "   branch [-d <branch>]")
  (println "   cat-file {-p|-t} <address>")
  (println "   commit <tree> -m \"message\" [(-p parent)...]")
  (println "   commit-tree <tree> -m \"message\" [(-p parent)...]")
  (println "   explore [-p <port>]")
  (println "   hash-object [-w] <file>")
  (println "   help")
  (println "   init")
  (println "   log --oneline [-n <count>] [<ref>]")
  (println "   rev-list [-n <count>] [<ref>]")
  (println "   rev-parse <ref>")
  (println "   switch [-c] <branch>")
  (println "   write-wtree"))

(defn help-error []
  (println "idiot help: print help for a command\n")
  (println "Usage: idiot help <command>\n")
  (println "Arguments:")
  (println "   <command>   the command to print help for\n")
  (println "Commands:")
  (println "   branch [-d <branch>]")
  (println "   cat-file {-p|-t} <address>")
  (println "   commit <tree> -m \"message\" [(-p parent)...]")
  (println "   commit-tree <tree> -m \"message\" [(-p parent)...]")
  (println "   explore [-p <port>]")
  (println "   hash-object [-w] <file>")
  (println "   help")
  (println "   init")
  (println "   log --oneline [-n <count>] [<ref>]")
  (println "   rev-list [-n <count>] [<ref>]")
  (println "   rev-parse <ref>")
  (println "   switch [-c] <branch>")
  (println "   write-wtree"))

(defn read-arg-help [{:keys [arg]}]
  ;;changing arg type from sequence to string

  (let [arg-s (first arg)]
    (cond
      (= arg-s nil) (top-level-error)
      (= arg-s "help") (help-error)
      (= arg-s "init") (init/init-error)
      (= arg-s "hash-object") (hash-object/hash-error)
      (= arg-s "cat-file") (cat-file/cat-error)
      (= arg-s "write-wtree") (write-wtree/wtree-error)
      (= arg-s "commit-tree") (commit-tree/commit-tree-er)
      (= arg-s "commit") (commit/commit-error)
      (= arg-s "rev-parse") (rev_parse/rev-parse-er)
      (= arg-s "switch") (switch/switch-er)
      (= arg-s "branch") (branch/branch-er)
      (= arg-s "rev-list") (revList/revHelp)
      (= arg-s "log") (log/logHelp)
      (or (= arg-s "-h") (= arg-s "--help")) (help-error)
      :else (println "Error: invalid command"))))

(defn run-com [{:keys [com dir db arg]}]
  (cond
    (= com "help") (read-arg-help {:arg arg})
    (= com "init") (init/read-arg-init {:arg arg :dir dir :db db})
    (= com "hash-object") (hash-object/read-arg-ho {:arg arg :dir dir :db db})
    (= com "cat-file") (cat-file/read-arg-cf {:arg arg :dir dir :db db})
    (= com "write-wtree") (write-wtree/write-wtree {:arg arg :dir dir :db db})
    (= com "commit-tree") (commit-tree/commit-tree {:arg arg :dir dir :db db :com com})
    (= com "commit") (commit-tree/commit-tree {:arg arg :dir dir :db db :com com})
    (= com "rev-parse") (rev_parse/rev-parse {:arg arg :dir dir :db db})
    (= com "switch") (switch/switch {:arg arg :dir dir :db db})
    (= com "branch") (branch/branch {:arg arg :dir dir :db db})
    (= com "rev-list") (revList/rev-list {:arg arg :dir dir :db db})
    (= com "log") (log/log {:arg arg :dir dir :db db})
    (or (= com nil) (= com "-h") (= com "--help")) (top-level-error)
    :else (println "Error: invalid command")))

(defn -main [& args]
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