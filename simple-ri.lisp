(in-package :cl-random-indexing)

(defclass simple-ri ()
  ((k :initarg :k :reader k)
   (d :initarg :d :reader d)
   (context-space :initform (make-hash-table :test 'equal) 
                  :reader context-space)))

(defmethod context (word (ri simple-ri))
  (gethash word (context-space ri)))

(defmethod (setf context) (context word (ri simple-ri))
  (setf (gethash word (context-space ri)) context))
  