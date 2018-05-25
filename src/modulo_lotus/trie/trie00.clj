(ns modulo-lotus.trie.trie00
  (:require [clojure.string :refer (split triml)]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

;; Naive solution
;; Counts the number of words beginning with a substring each time

(defn add [db name]
  (update-in db (seq name) (fnil assoc {}) :* true))

(defn count-terminations [db]
  (let [terminations (if (:* db) 1 0)]
    (reduce +
            terminations
            (map count-terminations
                 (vals (dissoc db :*))))))

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
           db {}]
      (when (> i 0)
        (let [[op contact] (split (read-line) #"\s+")]
          (recur (dec i)
                 (process-op db op contact)))))))