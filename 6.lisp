(require :uiop)

(defparameter *eg-lines* (uiop:read-file-lines "6.example"))

(defun split-grid (lines)
  (mapcar
    (lambda (line) (remove-if (lambda (el) (string= el "")) (uiop:split-string line)))
    lines))

(defun parse-nums (numeric-lines)
  (loop for line in numeric-lines collect (mapcar #'parse-integer line))) 

(defun do-col (i numeric-lines fn)
  (apply fn (mapcar (lambda (line) (elt line i)) numeric-lines)))

(defun op (name) (ecase (elt name 0) (#\+ #'+) (#\* #'*)))

(defun part-1 (lines)
  (let* ((grid (split-grid lines))
         (numeric-lines (parse-nums (butlast grid)))
         (ops (mapcar #'op (car (last grid)))))
    (loop for i from 0 below (length (first grid))
          summing (do-col i numeric-lines (elt ops i)))))

(part-1 (uiop:read-file-lines "6.example"))
(part-1 (uiop:read-file-lines "6.input"))


