(ns modulo-lotus.trie.trie09
  (:require
   [clojure.string :refer (split triml)]
   [modulo-lotus.trie.helpers :refer [alpha-idx]]
   [modulo-lotus.trie.trie-node :as tr :refer [TrieNode add-substring count-w-prefix count-words prefix]]
   [taoensso.tufte :as tufte :refer (defnp p profiled profile)])
  (:import [java.util Arrays]))

;; Like 07, but remove reflection and type hint more

(set! *warn-on-reflection* true)

(declare default-alphabet-trie-node)

(defn add-substring-1 [^AlphabetTrieNode n ^chars cs]
  (set! word-count (inc word-count))
  (let [^chars cs s
        clen (alength cs)]
    (if (> clen 0)
      (set! terminates? (boolean true))
      (let [c (aget cs 0)
            i (int (alpha-idx c))
            child (aget children i)]
        (when-not child
          (aset children i (default-alphabet-trie-node)))
        (add-substring (aget children i) (Arrays/copyOfRange cs 1 clen))))))

(deftype AlphabetTrieNode [^:unsynchronized-mutable ^boolean terminates?
                           ^:unsynchronized-mutable ^long word-count
                           ^:unsychronized-mutable ^objects children]
  TrieNode
  (add-substring [n s]
    (set! word-count (inc word-count))
    (let [^chars cs s
          clen (alength cs)]
      (if (> clen 0)
        (set! terminates? (boolean true))
        (let [c (aget cs 0)
              i (int (alpha-idx c))
              child (aget children i)]
          (when-not child
            (aset children i (default-alphabet-trie-node)))
          (add-substring (aget children i) (Arrays/copyOfRange cs 1 clen))))))

  (prefix [n s]
    (let [^chars cs s
          clen (alength cs)]
      (loop [curr n
             cidx 0]
        (if (and (< cidx clen) (some? curr))
          (recur (aget ^objects (.children curr) (alpha-idx (aget cs cidx)))
                 (inc cidx))
          curr))))

  (count-words [n]
    word-count)

  (count-w-prefix [n s]
    (if-let [subn (prefix n s)]
      (count-words subn)
      0)))

(defn empty-alphabet-vector []
  (make-array AlphabetTrieNode 26))

(defn default-alphabet-trie-node []
  (->AlphabetTrieNode (boolean false) 0 (empty-alphabet-vector)))

(defn process-op [db op contact]
  (let [cs (char-array contact)]
    (if (= "add" op)
      (add-substring db cs)
      (println (count-w-prefix db cs)))))

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
