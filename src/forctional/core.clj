(ns forctional.core
  "Supplementary API.")

(defn between?
  "Checks whether `lower <= c <= upper` is true."
  [c lower upper]
  (and
   (>= (compare c lower) 0)
   (<= (compare c upper) 0)))

(defn matches?
  "Checks whether the string `s` matches against the RegEx Pattern `re`."
  [s re]
  (-> (re-matcher re s)
      re-find
      nil?
      not))

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

(defn throws?
  "Invokes the provided `f` to check whether it throws an exception."
  [f]
  (try
    (not (any? (f)))
    (catch Exception _ true)))
