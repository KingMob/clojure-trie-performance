(ns modulo-lotus.trie.trie03
  (:require [clojure.string :refer (split triml)]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

;; From: https://www.hackerrank.com/challenges/ctci-contacts/submissions/code/46153278

;; Like 02, but swap reduce for transduce

(defn add [db name]
  (update-in db (seq name) (fnil assoc (array-map)) :* {}))

(defn count-terminations [db]
  (let [terminations (if (:* db) 1 0)]
    (transduce
     (map count-terminations)
     +
     terminations
     (vals db))))

(defn find-partial [db partial]
  (println
   (if-let [sub-db (get-in db (seq partial))]
     (count-terminations sub-db)
     0)))

(defn process-op [db op contact]
  (condp = op
    "add" (add db contact)
    "find" (do
             (find-partial db contact)
             db)))

(defn run
  []
  (let [n (Integer/parseInt (read-line))]
    (loop [i n
           db (array-map)]
      (when (> i 0)
        (let [[op contact] (split (read-line) #"\s+")]
          (recur (dec i)
                 (process-op db op contact))))))
  (flush))
