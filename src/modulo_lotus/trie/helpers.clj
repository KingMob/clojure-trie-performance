(ns modulo-lotus.trie.helpers)

(defn alpha-idx
  "Converts a letter to a 0-based index"
  [c]
  (- (int c) (int \a)))

(defn update-in!
  "Mimics update-in for transient collections.

      'Updates' a value in a nested associative structure, where ks is a
      sequence of keys and f is a function that will take the old value
      and any supplied args and return the new value, and returns a new
      nested structure.  If any levels do not exist, hash-maps will be
      created."
  {:added "1.0"
   :static true}
  ([m ks f & args]
   (let [up (fn up [m ks f args]
              (let [[k & ks] ks]
                #_(println m [k ks])
                (if ks
                  (assoc! m k (up (get m k (transient {})) ks f args))
                  (assoc! m k (apply f (get m k) args)))))]
     (up m ks f args))))