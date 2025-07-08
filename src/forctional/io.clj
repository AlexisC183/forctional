(ns forctional.io
  "Supplementary I/O API.")

(defn lines
  "Returns a sequence with the lines from a file read with the provided
  `java.io.BufferedReader` as `in`.
  See also [[clojure.java.io/reader]]."
  [in]
  (->> (repeat #(.readLine in))
       (map #(%))
       (take-while #(not (nil? %)))))
