(in-package #:restful)


(defclass resource ()
  ((identifier :initarg :identifier)
   (parent :initarg :parent
           :reader parent)))

(defgeneric view-document (resource)
  (:documentation "This function should return an object that will be
serialized to json using the jonathan library."))

(defmethod view-document ((resource resource))
  (slot-value resource 'identifier))
