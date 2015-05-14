(in-package #:restful)


(defvar *memory-storage-items* (make-hash-table :test 'equal))

(defclass memory-storage (storage)
  ())

(defmethod get-items ((storage memory-storage))
  (a:hash-table-values *memory-storage-items*))

(defmethod get-item ((storage memory-storage) identifier)
  (gethash identifier *memory-storage-items*))

(defmethod delete-item ((storage memory-storage) identifier)
  (remhash identifier *memory-storage-items*))

(defmethod save-item ((storage memory-storage) resource)
  (let ((result (view-resource resource)))
    (setf (gethash (getf result :identifier) *memory-storage-items*) result)))
