(require :uiop)

(defparameter *eg* "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(defun parse-range (range) (mapcar #'parse-integer (uiop:split-string range :separator "-")))
(defun parse-input (in) (mapcar #'parse-range (uiop:split-string in :separator ",")))
    
(defun n-digits (n) (1+ (floor (log n 10))))
(defun pad-right (n n-digits) (* n (expt 10 n-digits)))
(defun make-xx (n) (+ (pad-right n (n-digits n)) n))
(defun half-digits (n) (floor n (expt 10 (floor (n-digits n) 2))))

  

(defun in-range-p (range n) (and (>= n (first range)) (<= n (second range))))

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
  "List all the XXs in the single-order range (ie range must be split with
  split-range first)"
  (if (= 0 (mod (1+ (order (first range))) 2))
      (loop for a
            from (half-digits (first range))
            to (half-digits (second range))
            if (in-range-p range (make-xx a))
            collect (make-xx a))
      ; odd range, no xxs
      (list)))

  

(defun xx-in-range (range) "list XXs in arbitrary range" (mapcan #'xx-in-order-range (split-range range)))

(defun part-1 (input)
 (let*
     ((xxs (loop for range in (parse-input input) nconc (xx-in-range range)))
      (sum (loop for xx in xxs sum xx)))
   sum))

(part-1 *eg*)
(part-1 (uiop:read-file-string "2.input"))

; part 2
; for each range with normalize order-of-magnitude (start and and have same # digits):
;     for each numeric divisor of # digits:
;         take first `divisor` digits of start -> less-digits-start
;         take first `divisor` digits of end -> less-digits-end
;         loop from less-digits-start to less-digits-end:
;             repeat these digits x times,
;             so for divisor of 3 and #digits 6,
;             repeat twice 23 => 232323 to equal number of digits from range
;             discard if outside original range
;             collect into list of hits
; deduplicate list of hits and sum

(defun divisors-without-1 (n)
 (cdr (sort
        (remove-duplicates
           (loop for a from 1 to (ceiling (sqrt n))
                  if (= (rem n a) 0)
                  nconc (list a (/ n a))))
        #'<)))

(defun first-digits (n cnt) (floor n (expt 10 (- (n-digits n) cnt))))
(defun repeat-digits (n cnt)
 (loop for i
       from 0
       below (* cnt (n-digits n))
       by (n-digits n)
       summing (pad-right n i)))

(defun xn-in-order-range (range repeats)
  (loop for a
        from (first-digits (first range) (floor (n-digits (first range)) repeats))
        to (first-digits (second range) (floor (n-digits (second range)) repeats))
        if (in-range-p range (repeat-digits a repeats))
        collect (repeat-digits a repeats)))

(defun all-xn-in-order-range (range)
  (loop for repeats in (divisors-without-1 (n-digits (first range)))
        nconc (xn-in-order-range range repeats)))


(defun xn-in-range (range) (mapcan #'all-xn-in-order-range (split-range range)))

(defun part-2 (input)
  (let* ((xns (loop for range in (parse-input input) nconc (xn-in-range range)))
         (deduped-xns (remove-duplicates xns))
         (sum (loop for xn in deduped-xns sum xn)))
    sum))

(part-2 *eg*)
(part-2 (uiop:read-file-string "2.input"))
