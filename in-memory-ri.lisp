(in-package :cl-random-indexing)

(defclass in-memory-ri ()
  ((k :initarg :k :reader k)
   (d :initarg :d :reader d)
   (concept-space :initform (make-hash-table :test 'equal) 
                  :reader concept-space)))

(defmethod concept (word (ri in-memory-ri))
  (gethash word (concept-space ri)))

(defmethod (setf concept) (concept word (ri in-memory-ri))
  (setf (gethash word (concept-space ri)) concept))
  