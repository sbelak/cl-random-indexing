(in-package :cl-random-indexing)

(defun make-index (k d)
  (pairlis (n-samples k d) (generate k (lambda () (1- (* 2 (random 2)))))))

(defun make-concept (size)
  (make-array size :initial-element 0 :element-type 'fixnum))
    
(defun +index (concept index &optional (scale 1))
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) concept)
           (type fixnum scale))
  (dolist (i index concept)
    (declare (type (cons fixnum fixnum) i))
    (incf (aref concept (car i)) (the fixnum (* scale (cdr i))))))
        
(defun +concept (context concept)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) context concept))
  (assert (= (length concept) (length context)))
  (dotimes (i (length concept) context)
    (incf (aref context i) (aref concept i))))

(defgeneric concept (word ri))
(defgeneric (setf concept) (concept word ri))
(defgeneric d (ri))
(defgeneric k (ri))

(defun context (wordbag ri)
  (reduce (lambda (context word) (+concept context (concept word ri)))
          wordbag :initial-value (make-concept (d ri))))

(defun add-concept (word ri)
  (setf (concept word ri) (make-concept (d ri))))

(defun add-context (wordbag ri)
  (let ((index (make-index (k ri) (d ri))))
    (map nil (lambda (word)
               (+index (or (concept word ri) (add-concept word ri)) index))
         wordbag)
    ri))
