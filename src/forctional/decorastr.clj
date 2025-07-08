(ns forctional.decorastr
  "String-to-string functions.")

(defn surround-with-html
  "Encloses the provided `s` in HTML tags."
  [s]
  (str "<html>" s "</html>"))
