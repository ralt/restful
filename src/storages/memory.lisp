(in-package #:restful)


(defclass memory-storage (storage)
  ((storage :reader storage :initform (make-hash-table :test 'equal))))

(defmethod get-items ((storage memory-storage))
  (a:hash-table-values (storage storage)))

(defmethod get-item ((storage memory-storage) identifier)
  (gethash identifier (storage storage)))

(defmethod delete-item ((storage memory-storage) identifier)
  (remhash identifier (storage storage)))

(defmethod save-item ((storage memory-storage) resource)
  (let ((result (view-resource resource)))
    (setf (gethash (getf
                    result
                    (normalize-keyword
                     (intern
                      (symbol-name
                       (find-identifier-slot
                        (class-name (class-of resource))))
                      :keyword)))
                   (storage storage))
          result)))
