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

(defun op (name) (case (elt name 0) (#\+ #'+) (#\* #'*)))
(defun op-char (chr) (case chr (#\+ #'+) (#\* #'*)))

(defun part-1 (lines)
  (let* ((grid (split-grid lines))
         (numeric-lines (parse-nums (butlast grid)))
         (ops (mapcar #'op (car (last grid)))))
    (loop for i from 0 below (length (first grid))
          summing (do-col i numeric-lines (elt ops i)))))

(part-1 (uiop:read-file-lines "6.example"))
(part-1 (uiop:read-file-lines "6.input"))

(defun collect-column (numeric-lines i)
  "given a column index from the numeric lines, return the integer it represents, or nil if empty"
  (let* ((column (concatenate 'string (loop for line in numeric-lines collect (elt line i))))
         (trimmed (string-trim '(#\space) column)))
    (and (/= 0 (length trimmed)) (parse-integer trimmed))))

(defun part-2 (lines) ;imperative-ass code :'/
  (let ((numeric-lines (butlast lines))
        (op-line (car (last lines)))
        (num-cols (length (elt lines 0)))
        (current-op nil)
        (operands (list))
        (acc 0))
    (loop for i from 0 below num-cols do
          (let ((maybe-op (op-char (elt op-line i)))) (when maybe-op (setf current-op maybe-op)))
          (let ((col (collect-column numeric-lines i)))
            (when col (push col operands))
            (when (or (not col) (= i (1- num-cols)))
              (incf acc (apply current-op operands))
              (setf operands (list)))))
    acc))

(part-2 (uiop:read-file-lines "6.example"))
(part-2 (uiop:read-file-lines "6.input"))

