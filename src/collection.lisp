(in-package #:restful)


(defclass collection ()
  ((parent :initarg :parent
           :reader parent)))

(defgeneric view-collection (collection)
  (:documentation "This function should return an object that will be
serialized to json using the jonathan library."))

(defmethod view-collection ((collection collection)))
