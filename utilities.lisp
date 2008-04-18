(in-package :cl-random-indexing)

(defun generate (size generator)
  (loop repeat size collect (funcall generator)))

(defun n-samples (n limit)
  (assert (or (typep limit 'real) (< n limit)))
  (do ((samples () (adjoin (random limit) samples)))
      ((= (length samples) n) samples)))
       
(defun map-ngram (n fn wordbag)
  (loop with len = (length wordbag) with n = (min n len) for i from n to len 
        do (funcall fn (subseq wordbag (- i n) i))))
        
(defmacro do-ngram ((var n wordbag &optional result) &body body)
  `(block nil
     (map-ngram ,n (lambda (,var) ,@body) ,wordbag)
     ,result))
     
(defun cosine-similarity (a b)
  (declare (type (simple-array fixnum) a b))
  (assert (= (length a) (length b)))
  (locally (declare (optimize (speed 3) (safety 0)))
    (loop for ai across a for bi across b
          sum (the fixnum (* ai bi)) into dot-ab fixnum
          sum (the fixnum (* ai ai)) into L2-a fixnum
          sum (the fixnum (* bi bi)) into L2-b fixnum
          finally (return (/ dot-ab (max (sqrt L2-a) 1.0) 
                                    (max (sqrt L2-b) 1.0))))))
