(ns switch)


(defn switch-er []
      (println "idiot switch: change what HEAD points to

Usage: idiot switch [-c] <branch>

Arguments:
   -c   create the branch before switching to it"))


(defn switch [{:keys [arg dir db]}]
      (cond
        (or (= (first arg) "-h") (= (first arg) "--help")) (switch-er)
        (= nil arg) (println "Error: you must specify a branch name.")
        (= (first arg) "-c") (let [branch (rest arg)]
                                  (cond
                                    (= '() branch) (println "Error: you must specify a branch name.")
                                    (> (count branch) 1) (println "Error: you may only specify one branch name.")
                                    (not (.exists (io/file (str dir File/separator db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
                                    :else (let [branch (first branch)
                                                branch-path (str dir File/separator db File/separator "refs" File/separator "heads" File/separator branch)]
                                               (cond
                                                 (.exists (io/file branch-path)) (println "Error: a ref with that name already exists.")
                                                 :else (let [head-path (str dir File/separator db File/separator "HEAD")
                                                             head-cont (slurp head-path)
                                                             ref-path (str dir File/separator db File/separator (apply str (butlast (apply str (subs head-cont 5)))))
                                                             commit-addr (slurp ref-path)]
                                                            (spit branch-path commit-addr)
                                                            (spit head-path (str "ref: refs/heads/" branch "\n"))
                                                            (println (format "Switched to a new branch '%s'" branch)))))))
        (not= (first arg) "-c") (let [branch arg]
                                     (cond
                                       (> (count branch) 1) (println "Error: you may only specify one branch name.")
                                       (not (.exists (io/file (str dir File/separator db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
                                       :else (let [branch (first branch)
                                                   branch-path (str dir File/separator db File/separator "refs" File/separator "heads" File/separator branch)]
                                                  (cond
                                                    (not (.exists (io/file branch-path))) (println "Error: no ref with that name exists.")
                                                    :else (let [address (str dir File/separator db File/separator "HEAD")
                                                                text (str "ref: refs/heads/" branch "\n")]
                                                               (spit address text)
                                                               (println (format "Switched to branch '%s'" branch)))))))))
