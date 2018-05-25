(ns modulo-lotus.trie.trie05
  (:require
   [clojure.string :refer (split triml)]
   [modulo-lotus.trie.helpers :refer [alpha-idx]]
   [modulo-lotus.trie.trie-node :as tr :refer [TrieNode add-substring count-w-prefix count-words prefix]]
   [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

;; From: https://www.hackerrank.com/challenges/ctci-contacts/submissions/code/46671543

;; Like 04, but start storing word count at each node

(declare default-alphabet-trie-node)

(def empty-alphabet-vector (vec (repeat 26 nil)))

(defrecord AlphabetTrieNode [val terminates? word-count children]
  TrieNode
  (add-substring [n [c & cs :as s]]
    (->AlphabetTrieNode
     val
     (if c terminates? true)
     (inc word-count)
     (if c
       (update children
               (alpha-idx c)
               (fnil #(add-substring % cs)
                     (default-alphabet-trie-node c)))
       children)))

  (prefix [n s]
    (->> s
         (seq)
         (map alpha-idx)
         (interpose :children)
         (cons :children)
         (get-in n)))

  (count-words [n]
    word-count)

  (count-w-prefix [n s]
    (if-let [subn (prefix n s)]
      (count-words subn)
      0)))

(def default-alphabet-trie-node
  (memoize
   (fn [val]
     (->AlphabetTrieNode val false 0 empty-alphabet-vector))))

(defn process-op [db op contact]
  (if (= "add" op)
    (add-substring db contact)
    (do
      (println (count-w-prefix db contact))
      db)))

(defn run
  []
  (let [n (Integer/parseInt (read-line))]
    (loop [i n
           db (default-alphabet-trie-node 0)]
      (when (> i 0)
        (let [[op contact] (split (read-line) #"\s+")]
          (recur (dec i)
                 (process-op db op contact))))))
  (flush))
