(ns modulo-lotus.trie.core
  (:require [clojure.java.io :as io]
            [criterium.core :as crit]
            [cuerdas.core :as str]
            [modulo-lotus.trie.trie00 :as trie00]
            [modulo-lotus.trie.trie01 :as trie01]
            [modulo-lotus.trie.trie02 :as trie02]
            [modulo-lotus.trie.trie03 :as trie03]
            [modulo-lotus.trie.trie04 :as trie04]
            [modulo-lotus.trie.trie05 :as trie05]
            [modulo-lotus.trie.trie06 :as trie06]
            [modulo-lotus.trie.trie07 :as trie07]
            [modulo-lotus.trie.trie08 :as trie08]
            [taoensso.tufte :as tufte :refer [defnp p profiled profile]]))

(def ^:const input-file "add-max-then-find.txt")
(def ^:const default-num-runs 5)

(defn -main
  [i & [n]]
  (tufte/add-basic-println-handler! {})
  (println "Running version #" i)
  (let [run-fn (resolve (symbol (str "modulo-lotus.trie.trie"
                                     (str/pad i {:length 2 :padding "0"})
                                     "/run")))
        num-runs (if n (str/parse-int n) default-num-runs)]
    (if run-fn
      (do
        (println "Starting" num-runs "runs...")
        (profile {}
                 (dotimes [_ num-runs]
                   (binding [clojure.core/*flush-on-newline* false
                             clojure.core/*in* (io/reader (io/resource input-file))
                             clojure.core/*out* (io/writer "/dev/null")]
                     (p :run
                        (run-fn))))))
      (println "Could not find (run); did you require the ns?")))
  (Thread/sleep 500)
  (flush))