(ns ^:deprecated com.github.alexisc183.forctional.core
  "Utility library.")

(defn between?
  "Checks whether `lower <= c <= upper` is true."
  [c lower upper]
  (and
   (>= (compare c lower) 0)
   (<= (compare c upper) 0)))

(defn matrix?
  "Checks whether `x` is a rectangular matrix.
  In this context, a rectangular matrix is such if it is a vector of vectors
  where all the subvectors share the same length."
  [x]
  (cond
    (not (vector? x)) false
    (empty? x) true
    (some #(not (vector? %)) x) false
    :else (let [first-vec-count (count (first x))]
            (if (some #(not= (count %) first-vec-count) x)
              false
              true))))

(defn diagonal
  "Returns a sequence with the items of the main diagonal of `matrix`.
  See also [[matrix?]]."
  [matrix]
  (cond
    (not (matrix? matrix)) (throw (IllegalArgumentException. "matrix is not a vector of vectors with shared length."))
    (empty? matrix) (lazy-seq [])
    :else (->> (range 0 (min (count matrix) (count (first matrix))))
               (map #(-> matrix (nth %) (nth %))))))

(defn lines
  "Returns a sequence with the lines from a file read with the provided
  `java.io.BufferedReader` as `in`.
  See also [[clojure.java.io/reader]]."
  [in]
  (->> (repeat #(.readLine in))
       (map #(%))
       (take-while #(not (nil? %)))))

(defn matches?
  "Checks whether the string `s` matches against the RegEx Pattern `re`."
  [s re]
  (-> (re-matcher re s)
      re-find
      nil?
      not))

(defn mode
  "Determines the most common items from the provided `coll`."
  [coll]
  (cond
    (not (coll? coll)) (throw (IllegalArgumentException. "coll must be a collection"))
    (empty? coll) (lazy-seq [])
    :else (let [entries (->> coll
                             (group-by identity)
                             (map (fn [[k vs]] [k (count vs)]))

                             ;; Coll of [k count]s.
                             (sort #(- (compare (last %1) (last %2)))))]
            (->> entries
                 (filter #(= (last %) (-> entries first last)))
                 (map first)))))


(defn ^:deprecated nthable-coll?
  "Checks whether `x` is a vector, list or a sequence."
  [x]
  (or
   (vector? x)
   (list? x)
   (seq? x)))

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

(defn ^{:deprecated true
        :superseded-by "between?"} within-closed-range?
  "Checks whether `lower-bound <= comparable <= upper-bound` is true."
  [comparable lower-bound upper-bound]
  (and
   (>= (compare comparable lower-bound) 0)
   (<= (compare comparable upper-bound) 0)))
