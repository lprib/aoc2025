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

(defun row-slice (a row)
  (make-array
    (array-dimension a 1)
    :displaced-to a
    :displaced-index-offset (* row (array-dimension a 1))))

(defun split-stream (in-streams splitter-row)
  (let*
      ((w (length in-streams))
       (out-streams (make-list w :initial-element nil))
       (n-splits 0))
    (loop
      :for split-p :across splitter-row
      :for in-stream-p :in in-streams
      :for i :from 0
      :when (and in-stream-p (not split-p)) :do
        (setf (elt out-streams i) t)
        :end
      :when (and in-stream-p split-p) :do
        (setf (elt out-streams (1- i)) t)
        (setf (elt out-streams (1+ i)) t)
        (incf n-splits)
        :end)
    (values out-streams n-splits)))

(defun part-1 (lines)
  (multiple-value-bind (grid start-x) (parse-grid lines)
    (let* ((n-splits 0)
           (w (array-dimension grid 1))
           (streams (make-list w :initial-element nil)))
      (setf (elt streams start-x) t)
      (loop :for row :from 0 :below (array-dimension grid 0) :do
            (multiple-value-bind
                (new-streams splits-this-row)
                (split-stream streams (row-slice grid row))
              (setf streams new-streams)
              (incf n-splits splits-this-row)))
      n-splits)))

(defun split-stream-multiverse (in-streams splitter-row)
  (let*
      ((w (length in-streams))
       (out-streams (make-list w :initial-element 0)))
    (loop
      :for split-p :across splitter-row
      :for in-stream :in in-streams
      :for i :from 0
      :when (not split-p) :do
        (incf (elt out-streams i) in-stream)
        :end
      :when (and (/= 0 in-stream) split-p) :do
        (incf (elt out-streams (1- i)) in-stream)
        (incf (elt out-streams (1+ i)) in-stream)
        :end)
    out-streams))

(defun part-2 (lines)
  (multiple-value-bind (grid start-x) (parse-grid lines)
    (let* ((w (array-dimension grid 1))
           (streams (make-list w :initial-element 0)))
      (setf (elt streams start-x) 1)
      (loop :for row :from 0 :below (array-dimension grid 0)
            :do
            (let ((new-streams (split-stream-multiverse streams (row-slice grid row))))
              (setf streams new-streams)))
      (apply #'+ streams))))

(assert (= (part-1 (uiop:read-file-lines "7.example")) 21))
(assert (= (part-1 (uiop:read-file-lines "7.input")) 1658))

(assert (= (part-2 (uiop:read-file-lines "7.example")) 40))
(assert (= (part-2 (uiop:read-file-lines "7.input")) 53916299384254))


