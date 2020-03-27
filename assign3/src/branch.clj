(ns branch)

(defn branch-er []
  (println "idiot branch: list or delete branches

Usage: idiot branch [-d <branch>]

Arguments:
   -d <branch>   delete branch <branch>"))


(defn branch [{:keys [arg dir db]}]
  (cond
    (or (= (first arg) "-h") (= (first arg) "--help")) (branch-er)
    (and (= "-d" (first arg)) (= '() (rest arg))) (println "Error: you must specify a branch name.")
    (not (.exists (io/file (str dir File/separator db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
    (and (not= nil arg) (or (not= "-d" (first arg)) (not= 1 (count (rest arg))))) (println "Error: invalid arguments.")
    :else (let [[switch branch] arg
                branch-path (str dir File/separator db File/separator "refs" File/separator "heads" File/separator branch)]
            (cond
              (and (= "-d" switch) (not (.exists (io/file branch-path)))) (println (format "Error: branch '%s' not found." branch))
              :else (let [head-contents (slurp (str dir File/separator db File/separator "HEAD"))
                          head-branch (apply str (butlast (apply str (subs head-contents 16))))]
                      (cond
                        (and (= switch "-d") (= branch head-branch)) (println (format "Error: cannot delete checked-out branch '%s'." branch))
                        (= switch "-d") (do
                                          (.delete (io/file branch-path))
                                          (println (format "Deleted branch %s." branch)))
                        :else (let [refs (sort (.list (io/file (str dir File/separator db File/separator "refs" File/separator "heads"))))]
                                (doall (map (fn [ref]
                                              (cond
                                                (= ref head-branch) (println (format "* %s" ref))
                                                :else (println (format "  %s" ref)))) refs)))))))))
