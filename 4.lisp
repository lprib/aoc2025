(require :uiop)

(defun parse-input (lines)
  (let*
      ((w (length (elt lines 0)))
       (h (length lines))
       (arr (make-array (list h w)))) ; todo this has type error?
    (loop for y from 0 below h do
          (loop for x from 0 below w do
                (case (elt (elt lines y) x)
                  (#\@ (setf (aref arr (list y x)) 1))
                  (#\. (setf (aref arr (list y x)) 0)))))
    arr))
    

(make-array '(2 3))

(parse-input (uiop:read-file-lines "4.example"))
