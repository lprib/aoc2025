(require :uiop)

(defun parse-input (input)
  (let ((sep (position-if (lambda (line) (= 0 (length line))) input)))
    (list
      (parse-ranges (subseq input 0 sep))
      (parse-ingredients (subseq input (1+ sep))))))

(defun parse-ranges (range-input)
  (loop for range in range-input
        for halves = (uiop:split-string range :separator "-")
        collect (mapcar #'parse-integer halves)))
(defun parse-ingredients (ingredient-input) (mapcar #'parse-integer ingredient-input))

(defun in-range-p (range i) (and (>= i (first range)) (<= i (second range))))
(defun in-ranges-p (ranges i)
  (some (lambda (range) (in-range-p range i)) ranges))

(defun part-1 (ranges ingredients)
  (count-if (lambda (i) (in-ranges-p ranges i)) ingredients))

(apply #'part-1 (parse-input (uiop:read-file-lines "5.example")))
(apply #'part-1 (parse-input (uiop:read-file-lines "5.input")))

(defun max-upper-bound (ranges)
  (loop for range in ranges maximize (second range)))

(max-upper-bound (first (parse-input (uiop:read-file-lines "5.input"))))

(defun part-2-stupid (ranges) ; too slow
  (loop for a from 0 to (max-upper-bound ranges)
        count (in-ranges-p ranges a)))

(defparameter *egranges* (first (parse-input (uiop:read-file-lines "5.example"))))

(defun part-2 (ranges)
  (let
      ((ranges (sort ranges #'< :key #'first))
       (cnt 0)
       (i 0))
    (loop for (start end) in ranges do
          (when (< i start) (setf i start)) ; bump to start
          (unless (> i end)
            (let ((inc (1+ (- end i))))
              (incf i inc)
              (incf cnt inc))))
    cnt))
    
(part-2 *egranges*)
(part-2 (first (parse-input (uiop:read-file-lines "5.input"))))
