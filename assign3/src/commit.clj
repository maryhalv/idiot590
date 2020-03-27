(ns commit
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:import (java.io File))
  )

(defn commit-error []
  (println "idiot commit: create a commit and advance the current branch

Usage: idiot commit <tree> -m \"message\" [(-p parent)...]

Arguments:
   -h               print this message
   <tree>           the address of the tree object to commit
   -m \"<message>\"   the commit message
   -p <parent>      the address of a parent commit")
  )

(defn updateHead [{:keys [addr dir db]}]
  ;;if HEAD is pointing to a branch, update this branch with the commit info
  (let [head (slurp (io/file (str dir File/separator db File/separator "HEAD")))]
    (if (= (get (str/split head #":") 0) "ref")
      (let [ref (get (str/split head #"/heads") 1)]
        (cond
          (= ref "/master")
          (do
            (let [file (str dir File/separator db File/separator "refs" File/separator "heads" File/separator "master")]
              (io/make-parents file)
              (spit (io/file file) (str addr "\n"))
              )
            (println "Updated branch master.\n")
            )
          :else (let [subRef (subs ref 1 (- (count ref) 1))]
                   (let [file (str dir File/separator db File/separator "refs" File/separator "heads" File/separator subRef)]
                     (io/make-parents file)
                     (spit (io/file file) (str addr "\n"))
                     )
                   (println (str "Updated branch " subRef ".\n"))
                   )
          )
        )
      )
    )

  )


