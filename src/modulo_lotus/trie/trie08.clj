(ns modulo-lotus.trie.trie08
  (:require [clojure.string :refer (split triml)]
            [modulo-lotus.trie.helpers :refer [update-in!]]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

;; Like 01, but using transients

(defn inc-word-count [db name]
  (loop [db db
         [h & t] (seq name)
         path []]
    (if h
      (recur
       (update-in! db (conj path :word-count) (fnil inc 0))
       t
       (conj path h))
      db)))

(defn add [db name]
  #_(println "add/db" db)
  (-> db
      (update-in! (seq name) (fnil assoc! (transient {})) :* true)
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
           db (transient {})]
      #_(println "run/db" db)
      (if (> i 0)
        (let [[op contact] (split (read-line) #"\s+")]
          (recur (dec i)
                 (process-op db op contact)))))))