(in-package #:restful)


(defclass collection ()
  ((parent :initarg :parent
           :reader parent)
   (storage :initarg :storage
            :reader storage)))

(defgeneric view-collection (collection)
  (:documentation "This function should return an object that will be
serialized to json using the jonathan library."))

(defmethod view-collection ((collection collection)))
