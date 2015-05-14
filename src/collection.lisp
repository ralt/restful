(in-package #:restful)


(defclass collection ()
  ((parent :initarg :parent
           :reader parent)
   (storage :initarg :storage
            :reader storage)
   (class-of-resource :initarg :class-of-resource
                      :reader class-of-resource)))

(defgeneric view-collection (collection)
  (:documentation "This function should return an object that will be
serialized to json using the jonathan library."))

(defmethod view-collection ((collection collection))
  (populate-resources collection (get-items (storage collection))))

(defun populate-resources (collection items)
  (mapcar #'(lambda (item)
              (let ((resource
                     (make-instance (slot-value collection 'class-of-resource))))
                (populate-resource resource item)
                resource))
          items))
