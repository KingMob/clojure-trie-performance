(ns modulo-lotus.trie.trie-node)

(defprotocol TrieNode
  (add-substring [n s])
  (prefix [n s])
  (count-words [n])
  (count-w-prefix [n s]))