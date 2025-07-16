(ns forctional.decorastr
  "String-to-string functions."
  (:require [clojure.string :as str]))

(defn- escape-json-component
  [s]
  (when-not (instance? String s) (throw (ClassCastException. "s must be a String")))
  (-> s
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")
      (str/replace #"\R" (->> (System/lineSeparator)
                              (map #(if (= % \return)
                                      "\\\\r"
                                      "\\\\n"))
                              (reduce str)))))

(defn err-json
  "Returns a JSON string with \"err\" as key and `s` as value."
  [s]
  (str "{\"err\":\"" (escape-json-component s) "\"}"))

(defn out-json
  "Returns a JSON string with \"out\" as key and `s` as value."
  [s]
  (str "{\"out\":\"" (escape-json-component s) "\"}"))

(defn surround-with-html
  "Encloses the provided `s` in HTML tags."
  [s]
  (str "<html>" s "</html>"))
