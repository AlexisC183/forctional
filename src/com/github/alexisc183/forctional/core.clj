(ns com.github.alexisc183.forctional.core
  "Utility library.")

(defn nthable-coll?
  "Checks whether `x` is a vector, list or a sequence."
  [x]
  (or
   (vector? x)
   (list? x)
   (seq? x)))

(defn matrix?
  "Checks whether `x` is a rectangular matrix with cells accessible via `nth`.
  A rectangular matrix is a collection of collections where all the subcollections
  share the same length, in this way emulating a grid-like data structure."
  [x]
  (cond
    (not (nthable-coll? x)) false
    (empty? x) true
    
    ;; Starting from here x has at least 1 item.
    :else (loop [i 0]
            (cond
              (= i (count x)) true
              (or
               (not (nthable-coll? (nth x i)))
               (not= (count (nth x i)) (count (first x)))) false
              :else (recur (inc i))))))


(defn diagonal
  "Returns a collection with the items of the main diagonal of `matrix`.
  See also [[matrix?]]."
  [matrix]
  (if-not (matrix? matrix)
    (throw (IllegalArgumentException. "matrix cells cannot be accessed via nth or matrix is not rectangular"))
    (loop [i 0
           diagonal-items []]
      (if (or (= i (count matrix)) (= i (count (nth matrix i))))
        diagonal-items
        (recur
         (inc i)
         (conj diagonal-items (-> matrix
                                  (nth i)
                                  (nth i))))))))

(defn lines
  "Returns all the lines from a file read with the provided `reader_`.
  See also [[clojure.java.io/reader]]."
  [reader_]
  (loop [line (.readLine reader_)
         lines []]
    (if (nil? line)
      lines
      (recur (.readLine reader_) (conj lines line)))))

(defn matches?
  "Checks whether the string `s` matches against the RegEx Pattern `re`."
  [s re]
  (-> (re-matcher re s)
      re-find
      nil?
      not))

(defn mode
  "Determines the most common items from the provided `ncoll`."
  [ncoll]
  (cond
    (not (nthable-coll? ncoll)) (throw (IllegalArgumentException. "ncoll must be a vector, list or a sequence"))
    (empty? ncoll) []

    ;; ncoll is a non-empty coll.
    :else (let [entries (->> ncoll
                             (group-by identity)
                             (map (fn [[k vs]] [k (count vs)]))

                             ;; Coll of [k count]s.
                             (sort #(- (compare (last %1) (last %2)))))]
            (->> entries
                 (filter #(= (last %) (-> entries
                                          first
                                          last)))
                 (map first)))))


(defn surround-with-html
  "Encloses the provided `s` in HTML tags."
  [s]
  (str "<html>" s "</html>"))

(defn throws?
  "Invokes the provided `f` to check whether it throws an exception."
  [f]
  (try
    (not (any? (f)))
    (catch Exception _ true)))

(defn within-closed-range?
  "Checks whether `lower-bound <= comparable <= upper-bound` is true."
  [comparable lower-bound upper-bound]
  (and
   (>= (compare comparable lower-bound) 0)
   (<= (compare comparable upper-bound) 0)))
