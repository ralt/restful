(in-package #:restful)


(defclass collection ()
  ((parent :initarg :parent
           :reader parent
           :documentation "The parent resource, if any. If there's no
parent, its value is NIL.")
   (storage :initarg :storage
            :reader storage
            :documentation "The storage object that satisfies the
interface of the `restful:storage` class.")
   (class-of-resource
    :initarg :class-of-resource
    :reader class-of-resource
    :documentation "The resource's class of this collection."))
  (:documentation "Base class for restful collections. There's
not much reason to extend it with the current features."))

(defgeneric view-collection (collection)
  (:documentation "Returns an object that will be
serialized to json using the jonathan library."))

(defmethod view-collection ((collection collection))
  "Returns an object populated by the collection's storage."
  (populate-resources collection (get-items (storage collection))))

(defun populate-resources (collection items)
  (mapcar #'(lambda (item)
              (let ((resource
                     (make-instance (slot-value collection 'class-of-resource))))
                (populate-resource resource item)
                resource))
          items))
