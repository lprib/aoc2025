(require :uiop)

(defconstant +numbers+ '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(defun parse-banks (input)
  (mapcar
    (lambda (line)
      (mapcar
        (lambda (c) (position c +numbers+))
        (coerce line 'list)))
    input))

(defun idx-of-largest (bank) (position (reduce #'max bank) bank))

(defun joltage (bank)
  (let*
      ((bank-without-last (butlast bank))
       (largest-idx (idx-of-largest (butlast bank)))
       (bank-after-first (nthcdr (1+ largest-idx) bank))
       (second-largest-idx (idx-of-largest bank-after-first)))
    (+ (* 10 (nth largest-idx bank-without-last)) (nth second-largest-idx bank-after-first)))) 

(defun part-1 (input)
  (loop for bank in (parse-banks input) sum (joltage bank)))

(part-1 (uiop:read-file-lines "3.example"))
(part-1 (uiop:read-file-lines "3.input"))

; part-2 
; 12 digits of joltage
; first digit: search list[-12]
; rest = list[findpos..]
; second digit: search rest[-11] (need to leave space for at least 11 digits after)
; third digit: search rest[-10]

(defconstant +n-bats+ 12)
; 0 -> butlast 11


(loop for i from 0 below +n-bats+ collect (butlast *test* (- (1- +n-bats+) i)))
(print *test*)
(defparameter *test* '(8 1 1 1 1 1 1 1 1 1 1 1 1 1 9))

(defun joltage-2 (bank)
  (let*
      ((rest-of-bank (copy-list bank))
       (digits (list)))
    (loop for i from 0 below +n-bats+ do
          (let ((largest-idx (idx-of-largest (butlast rest-of-bank (- (1- +n-bats+) i)))))
            (push (nth largest-idx rest-of-bank) digits)
            (setf rest-of-bank (nthcdr (1+ largest-idx) rest-of-bank))))
    (nreverse digits)))

(defun make-num (digits)
  (loop for dig in (reverse digits)
        and i from 0
        sum (* dig (expt 10 i)))) 

   
(defun part-2 (input)
  (loop for bank in (parse-banks input) sum (make-num (joltage-2 bank))))

(part-2 (uiop:read-file-lines "3.example"))
(part-2 (uiop:read-file-lines "3.input"))
