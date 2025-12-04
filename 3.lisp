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
