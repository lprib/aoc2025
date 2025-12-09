(require :uiop)

(defun parse-jn (line)
  (mapcar #'parse-integer (uiop:split-string line :separator ",")))

(defun parse (filename)
  (mapcar #'parse-jn (uiop:read-file-lines filename)))

(defun circuits (points) (mapcar (lambda (p) (list p 0)) points))

(defparameter *eg* (circuits (parse "8.example")))

(defun sqr (n) (* n n))
(defun dist (a b)
  (sqrt (+
          (sqr (- (first a) (first b)))
          (sqr (- (second a) (second b)))
          (sqr (- (third a) (third b))))))

(defun different-circuit (c1 c2) (or (= c1 0) (= c2 0) (/= c1 c2)))

(defun min-pair (circuits)
  (let ((min-dist 99999999999)
        (idxs nil)
        (pts nil))
      (loop
        :for (p1 c1) :in points
        :for i1 :from 0
        :do
        (loop
          :for (p2 c2) :in points
          :for i2 :from 0
          :for pdist = (Dist p1 p2)
          :when (and (/= i1 i2) (different-circuit c1 c2) (< pdist min-dist))
            :do
            (setf min-dist pdist)
            (setf idxs (list i1 i2))
            (setf pts (list p1 p2))))
    (values idxs)))

(defun part-1 (circuits)
  (loop
    :for i :from 0 :below 1000 :do
    (multiple-value-bind)))
;;not really clocking it........how to connect circuits
    
        

(min-pair *eg*)
