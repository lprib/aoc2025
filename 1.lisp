(require :uiop)

(defun rotation (str)
  (let ((n (parse-integer (subseq str 1))))
   (case (elt str 0) ((#\L #\l) (- n)) ((#\R #\r) n))))

(defun accumulate-rotations (rots)
  (let ((a 50) (zeros 0))
    (dolist (rot rots)
      (when (and (= a 0) (< rot 0)) (setf zeros (1- zeros)))
      (setf a (+ a rot))
      (let ((clicked (passing-click zeros a)))
        (setf zeros (first clicked) a (second clicked))
        (when (and (>= 0 rot) (= a 0)) (setf zeros (1+ zeros)))))
    zeros))

(defun passing-click (zeros rot)
  (cond
    ((>= rot 100) (passing-click (1+ zeros) (- rot 100)))
    ((< rot 0) (passing-click (1+ zeros) (+ rot 100)))
    (t (list zeros rot))))
    
(let ((lines (uiop:read-file-lines "1.input")))
  (print (accumulate-rotations (mapcar #'rotation lines))))

(accumulate-rotations (mapcar #'rotation (list "L68" "L30" "R48" "L5" "R60" "L55" "L1" "L99" "R14" "L82")))

(accumulate-rotations (list -50 10 -10 -1 1))
(accumulate-rotations (list -51 1))
(accumulate-rotations (list -50 -1))

(passing-click 0 -1)
