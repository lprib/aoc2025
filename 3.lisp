(require :uiop)

(defconstant +numbers+ '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(defun parse-banks (input)
  (mapcar
    (lambda (line)
      (mapcar
        (lambda (c) (position c +numbers+))
        (coerce line 'list)))
    input))

(defparameter *egb* (parse-banks (uiop:read-file-lines "3.example")))

(defun all-positions (needle haystack)
  (loop
    for element in haystack
    and position from 0
    when (eql element needle)
    collect position))

(defun max-first-digit-idxs (bank)
  "return list of (pos next-digit)"
  (let* ((relevent-idxs (butlast bank))
         (max-digit (apply #'max relevent-idxs)))
    (mapcar (lambda (pos) (list pos (nth (1+ pos) bank))) (all-positions max-digit relevent-idxs))))

(defun max-second-digit-idxs (first-pairs)
  (reduce (lambda (a b) (if (> (second a) (second b)) a b)) first-pairs))

(let ((bank '(9 8 8 9 0 9)))
 (max-second-digit-idxs (max-first-digit-idxs bank)))

(defun max-pair (bank)
  (let ((index (first (max-second-digit-idxs (max-first-digit-idxs bank)))))
    (+ (* (nth index bank) 10) (nth (1+ index) bank))))
;; this shit is all wrong

(mapcar #'max-pair *egb*)

