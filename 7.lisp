(require :uiop)

(defun parse-grid (lines)
  "parse the grid. return 2d array of bool as first return, x-index of starting
  beam as second return"
  (let* ((h (length lines))
         (w (length (first lines)))
         (grid (make-array (list h w) :initial-element nil))
         (start-x 0))
    (loop for y from 0 below h do
          (loop for x from 0 below w
                for chr = (elt (elt lines y) x)
                do
                (setf
                  (aref grid y x)
                  (ecase chr (#\. nil) (#\S nil) (#\^ t)))
                (when (char= chr #\S)
                  (setf start-x x))))
    (values grid start-x)))

; given stream-arr (list of bool) which is where the stream is, and splitters
; which is list of bool where the splitters are, determine what the new stream
; is. Also need to count then number of splits somehow

(parse-grid (uiop:read-file-lines "7.example"))

(defparameter *a* (make-array '(3 2) :initial-element 0))

(defun row-slice (a row)
  (make-array
    (array-dimension a 1)
    :displaced-to a
    :displaced-index-offset (* row (array-dimension a 1))))

(row-slice *a* 2)

(defun elt-oob (seq i)
  (if (and (<= 0 i) (> (length seq) i)) (elt seq i) nil))

(defun split-single-row (streams splitter-row i)
  (or
    (and (elt streams i) (not (elt splitter-row i)))
    (and (elt-oob streams (1- i)) (elt-oob splitter-row (1- i)))
    (and (elt-oob streams (1+ i)) (elt-oob splitter-row (1+ i)))))
    

(defun split-stream (streams splitter-row)
  (loop
    for stream-p in streams
    for i from 0
    collect (split-single-row streams splitter-row i)))

(split-stream '(nil nil t nil nil) '(nil nil t nil nil))
; todo need to count the splits
