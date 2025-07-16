(ns forctional.servlets
  "Jakarta Servlet utilities."
  (:require [forctional.decorastr :as deco])
  (:import [java.io PrintWriter]))

(defn try-print
  "Prints with the provided `java.io.PrintWriter` as `out` the result of
  invoking `f`.
  If an exception is thrown, then an err JSON with the exception message is
  printed.
  See also [[forctional.decorastr/err-json]]."
  [out f]
  (cond
    (not (instance? PrintWriter out)) (throw (ClassCastException. "out must be a java.io.PrintWriter"))
    (not (fn? f)) (throw (IllegalArgumentException. "f must be a function"))
    :else (try
            (.print out (f))
            (catch Exception e
              (.print out (deco/err-json (.getMessage e)))))))
