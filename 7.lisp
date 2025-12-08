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

(defun split-stream (stream-arr splitters))
; given stream-arr (list of bool) which is where the stream is, and splitters
; which is list of bool where the splitters are, determine what the new stream
; is. Also need to count then number of splits somehow

(parse-grid (uiop:read-file-lines "7.example"))
