(ns modulo-lotus.trie.trie07
  (:require
   [clojure.string :refer (split triml)]
   [modulo-lotus.trie.helpers :refer [alpha-idx]]
   [modulo-lotus.trie.trie-node :as tr :refer [TrieNode add-substring count-w-prefix count-words prefix]]
   [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

;; Like 06, but remove val field, replace volatile-mutable with unsynchronized-mutable, switch to Java arrays and primitives

(set! *warn-on-reflection* true)

(declare default-alphabet-trie-node)

(deftype AlphabetTrieNode [^:unsynchronized-mutable terminates?
                           ^:unsynchronized-mutable ^long word-count
                           ^:unsychronized-mutable ^objects children]
  TrieNode
  (add-substring [n [c & cs]]
    (set! word-count (inc word-count))
    (if-not c
      (set! terminates? true)
      (let [i (int (alpha-idx c))
            child (aget children i)]
        (when-not child
          (aset children i (default-alphabet-trie-node)))
        (add-substring (aget children i) cs))))

  (prefix [n s]
    (loop [curr n
           [c & cs] s]
      (if (and c curr)
        (recur (nth (.children curr) (alpha-idx c))
               cs)
        curr)))

  (count-words [n]
    word-count)

  (count-w-prefix [n s]
    (if-let [subn (prefix n s)]
      (count-words subn)
      0)))

(defn empty-alphabet-vector []
  (make-array AlphabetTrieNode 26))

(defn default-alphabet-trie-node []
  (->AlphabetTrieNode false 0 (empty-alphabet-vector)))

(defn process-op [db op contact]
  (if (= "add" op)
    (add-substring db contact)
    (println (count-w-prefix db contact))))

(defn run
  []
  (let [n (Integer/parseInt (read-line))
        db (default-alphabet-trie-node)]
    (loop [i n]
      (when (> i 0)
        (let [[op contact] (split (read-line) #"\s+")]
          (process-op db op contact)
          (recur (dec i))))))
  (flush))
