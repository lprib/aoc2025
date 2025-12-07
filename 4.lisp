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
  
  

(let ((a (parse-input (uiop:read-file-lines "4.example"))))
  (print (num-adjacent a 0 0)))
