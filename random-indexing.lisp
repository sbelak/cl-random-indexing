(in-package :cl-random-indexing)

(defun make-index-vector (k d)
  (pairlis (n-samples k d) (generate k (lambda () (1- (* 2 (random 2)))))))

(defun make-context-vector (size)
  (make-array size :initial-element 0 :element-type 'fixnum))

(defun +index (context index)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) context))
  (dolist (pos-val index context)
    (incf (aref context (car pos-val)) (the fixnum (cdr pos-val)))))

(defun +context (concept context)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) concept context))
  (assert (= (length context) (length concept)))
  (dotimes (i (length context) concept)
    (incf (aref concept i) (aref context i))))

(defgeneric context (word ri))
(defgeneric (setf context) (context word ri))
(defgeneric d (ri))
(defgeneric k (ri))

(defun concept (wordbag ri)
  (reduce (lambda (concept word) (+context concept (context word ri)))
          wordbag :initial-value (make-context-vector (d ri))))

(defun add-word (word ri)
  (setf (context word ri) (make-context-vector (d ri))))

(defun add-context (wordbag ri)
  (let ((index (make-index-vector (k ri) (d ri))))
    (map nil (lambda (word)
               (+index (or (context word ri) (add-word word ri)) index))
         wordbag)
    ri))
