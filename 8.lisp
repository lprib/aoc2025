(require :uiop)

(defun parse-jn (line)
  (mapcar #'parse-integer (uiop:split-string line ",")))

(defun parse (filename)
  (mapcar #'parse-jn (uiop:read-file-lines filename)))


(parse "8.example")
