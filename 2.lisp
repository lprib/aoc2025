(require :uiop)

(defparameter *eg* "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(uiop:split-string *eg* :separator ",")

(defun parse-range (range) (mapcar #'parse-integer (uiop:split-string range :separator "-")))
(defun parse-input (in) (mapcar #'parse-range (uiop:split-string in :separator ",")))
    
(defun n-digits (n) (1+ (floor (log n 10))))
(defun pad-left (n n-digits) (* n (expt 10 n-digits)))
(defun make-xx (n) (+ (pad-left n (n-digits n)) n))
(defun half-digits (n) (floor n (expt 10 (floor (n-digits n) 2))))

(defun range-xx (range)
  (let ((xxs (list))(start (half-digits (first range))))))
    

(defparameter *s* 100)
(defparameter *e* 600)
(defparameter *range* (list 1000 6000))
(defun in-range (range n) (and (>= n (first range)) (<= n (second range))))
(loop for a from (half-digits (first *range*)) to (half-digits (second *range*)) if (in-range *range* (make-xx a)) collect (make-xx a))

(loop for a from 0 to 10 if (< a 5) collect a)
    
(defun split-range (range)
  "split range into sub-ranges of same number of digits
  e.g. 20-1002 -> 20-99 100-999 1000-1002")
