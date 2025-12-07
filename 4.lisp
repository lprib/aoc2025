(require :uiop)

(defun parse-input (lines)
  "lines is a list of strings"
  (let*
      ((w (length (elt lines 0)))
       (h (length lines))
       (arr (make-array (list h w)))) ; todo this has type error?
    (loop for y from 0 below h do
          (loop for x from 0 below w do
                (case (elt (elt lines y) x)
                  (#\@ (setf (aref arr y x) 1))
                  (#\. (setf (aref arr y x) 0)))))
    arr))

(defun aref-oob (a y x &key (default-value 0))
  (if (and
        (>= y 0)
        (>= x 0)
        (< y (array-dimension a 0))
        (< x (array-dimension a 1)))
      (aref a y x)
      default-value))

(defconstant +adj+
  (list '(-1 -1) '(0 -1) '(1 -1) '(-1 0) '(1 0) '(-1 1) '(0 1) '(1 1)))
    
(defun num-adjacent (a y x)
  (apply #'+ (mapcar (lambda (off) (aref-oob a (+ y (first off)) (+ x (second off)))) +adj+)))

(defun should-remove (arr y x)
  (and (= 1 (aref arr y x)) (< (num-adjacent arr y x) 4)))
  
(defun part-1 (lines)
  (let ((arr (parse-input lines)) (accessible 0))
    (loop for y from 0 below (array-dimension arr 0) do
          (loop for x from 0 below (array-dimension arr 1) do
                (when (should-remove arr y x)
                  (incf accessible))))
    accessible))

(defun find-removable (arr)
  (let ((to-remove (list)))
    (loop for y from 0 below (array-dimension arr 0) do
          (loop for x from 0 below (array-dimension arr 1) do
                (when (should-remove arr y x)
                  (push (list y x) to-remove))))
    (nreverse to-remove)))

(defun perform-removals (arr removals)
  (loop for (y x) in removals do
        (setf (aref arr y x) 0)))

(defun part-2 (lines)
  (let ((arr (parse-input lines)) (removed 0))
    (loop
      (let* ((removals (find-removable arr)))
        (if (> (length removals) 0)
            (progn
              (incf removed (length removals))
              (perform-removals arr removals))
            (return removed))))))
      
    
(part-1 (uiop:read-file-lines "4.example"))
(part-1 (uiop:read-file-lines "4.input"))

(part-2 (uiop:read-file-lines "4.example"))
(part-2 (uiop:read-file-lines "4.input"))
