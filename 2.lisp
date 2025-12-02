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
    

(defun in-range (range n) (and (>= n (first range)) (<= n (second range))))

(defun order (n) (floor (log n 10)))
    
(defun split-range (range)
  "split range into sub-ranges of same number of digits
  e.g. 20-1002 -> 20-99 100-999 1000-1002"
  (let
      ((order-1 (order (first range)))
       (order-2 (order (second range))))
    (if (= order-1 order-2)
        (list range)
        (nconc
          (split-range (list (first range) (1- (expt 10 order-2))))
          (list (list (expt 10 order-2) (second range)))))))

(defun xx-in-order-range (range)
  (if (= 0 (mod (1+ (order (first range))) 2))
      (loop for a
            from (half-digits (first range))
            to (half-digits (second range))
            if (in-range range (make-xx a))
            collect (make-xx a))
      ; odd range, no xxs
      (list)))

(defun xx-in-range (range)
  (mapcan #'xx-in-order-range (split-range range)))

(defun part-1 (input)
 (let*
     ((xxs (loop for range in (parse-input input) nconc (xx-in-range range)))
      (sum (loop for xx in xxs sum xx)))
   sum))

(part-1 *eg*)
(part-1 (uiop:read-file-string "2.input"))
