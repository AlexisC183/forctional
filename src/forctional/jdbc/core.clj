(ns forctional.jdbc.core
  "Higher-level JDBC API."
  (:import [forctional.jdbc JdbcManager]))

(defn- throw-if-bad-args
  ([mng sql]
   (cond
     (not (instance? JdbcManager mng)) (throw (ClassCastException. "mng must be a forctional.jdbc.JdbcManager"))
     (not (instance? String sql)) (throw (ClassCastException. "sql must be a String"))))
  ([mng sql values cols]
   (throw-if-bad-args mng sql)
   (cond
     (not (vector? values)) (throw (IllegalArgumentException. "values must be a vector"))
     (not (set? cols)) (throw (IllegalArgumentException. "cols must be a set")))))

(defn- result-set->vec
  [result-set key-name-entries]
  (->> (repeat result-set)
       (take-while #(.next %))
       (map (fn [rs] (->> key-name-entries
                          (map (fn [[k n]] [k (.getObject rs n)]))
                          (reduce (fn [m [k v]] (assoc m k v)) {}))))
       vec))

(defn manager
  "Creates an object that may auto-close the JDBC resources produced by the
  provided `javax.sql.DataSource` as `ds`."
  [ds]
  (JdbcManager. ds))

(defn exec
  "Executes the `sql` string with the JDBC manager `mng`.
  A `values` vector is used when `sql` has ? placeholders."
  ([mng sql]
   (throw-if-bad-args mng sql)
   (.execute mng sql)
   nil)
  ([mng sql values]
   (throw-if-bad-args mng sql)
   (when-not (vector? values) (throw (IllegalArgumentException. "values must be a vector")))
   (let [ps (.prepareStatement mng sql)
         object-count (count values)]
     (loop [i 0
            ordinal 1]
       (if (> ordinal object-count)
         (do (.execute mng ps) nil)
         (do
           (.setObject ps ordinal (nth values i))
           (recur (inc i) (inc ordinal))))))))

(defn exec-query
  "Returns the result of executing the `sql` string with the JDBC manager `mng`.
  `cols` is a set of keywords that match the names of the desired colums to
  retrieve.
  A `values` vector is used when `sql` has ? placeholders."
  ([mng sql cols]
   (throw-if-bad-args mng sql)
   (when-not (set? cols) (throw (IllegalArgumentException. "cols must be a set"))) 
   (result-set->vec (.executeQuery mng sql) (map (fn [k] [k (name k)]) cols)))
  ([mng sql values cols]
   (throw-if-bad-args mng sql values cols)
   (let [ps (.prepareStatement mng sql)
         object-count (count values)]
     (loop [i 0
            ordinal 1]
       (if (> ordinal object-count)
         (result-set->vec (.executeQuery mng ps) (map (fn [k] [k (name k)]) cols))
         (do
           (.setObject ps ordinal (nth values i))
           (recur (inc i) (inc ordinal))))))))
