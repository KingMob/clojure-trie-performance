(ns modulo-lotus.trie.trie06
  (:require
   [clojure.string :refer (split triml)]
   [modulo-lotus.trie.helpers :refer [alpha-idx]]
   [modulo-lotus.trie.trie-node :as tr :refer [TrieNode add-substring count-w-prefix count-words prefix]]
   [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

;; From: https://www.hackerrank.com/challenges/ctci-contacts/submissions/code/46682293

;; Like 05, but switch to deftype, and use volatile-mutable

(declare default-alphabet-trie-node)

(def empty-alphabet-vector (vec (repeat 26 nil)))

(deftype AlphabetTrieNode [val
                           ^:volatile-mutable terminates?
                           ^:volatile-mutable word-count
                           ^:volatile-mutable children]
  TrieNode
  (add-substring [n [c & cs]]
    (set! word-count (inc word-count))
    (if-not c
      (set! terminates? true)
      (let [i (alpha-idx c)
            child (children i)]
        (when-not child
          (->> c
               (default-alphabet-trie-node)
               (assoc children i)
               (set! children)))
        (add-substring (children i) cs))))

  (prefix [n s]
    (loop [curr n
           [c & cs] s]
      (if (and c curr)
        (recur (get (.children curr) (alpha-idx c))
               cs)
        curr)))

  (count-words [n]
    word-count)

  (count-w-prefix [n s]
    (if-let [subn (prefix n s)]
      (count-words subn)
      0)))

(defn default-alphabet-trie-node [val]
  (->AlphabetTrieNode val false 0 empty-alphabet-vector))

(defn process-op [db op contact]
  (if (= "add" op)
    (add-substring db contact)
    (println (count-w-prefix db contact))))

(defn run
  []
  (let [n (Integer/parseInt (read-line))
        db (default-alphabet-trie-node 0)]
    (loop [i n]
      (when (> i 0)
        (let [[op contact] (split (read-line) #"\s+")]
          (process-op db op contact)
          (recur (dec i))))))
  (flush))
