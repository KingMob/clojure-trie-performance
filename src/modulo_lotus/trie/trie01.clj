(ns modulo-lotus.trie.trie01
  (:require [clojure.string :refer (split triml)]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

;; Doesn't seem to have been submitted to Hackerrank - like the very first one, but with a word count at each node
;; Speeds things up by a factor of 10

(defn inc-word-count [db name]
  (loop [db db
         [h & t] (seq name)
         path []]
    (if h
      (recur
       (update-in db (conj path :word-count) (fnil inc 0))
       t
       (conj path h))
      db)))

(defn add [db name]
  (-> db
      (update-in (seq name) (fnil assoc (array-map)) :* true)
      (inc-word-count name)))

(defn find-partial [db partial]
  (println
   (if-let [sub-db (get-in db (seq partial))]
     (get sub-db :word-count 0)
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
      (if (> i 0)
        (let [[op contact] (split (read-line) #"\s+")]
          (recur (dec i)
                 (process-op db op contact)))))))