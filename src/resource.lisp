(in-package #:restful)


(defclass resource ()
  ((identifier :initarg :identifier)
   (parent :initarg :parent
           :reader parent)))

(defgeneric view-resource (resource)
  (:documentation "This function should return an object that will be
serialized to json using the jonathan library."))

(defgeneric load-resource (resource)
  (:documentation "This function loads a resource based on its identifier."))

(defgeneric create-resource (resource post-data)
  (:documentation "This function creates a new resource."))

(defgeneric patch-resource (resource post-data)
  (:documentation "This function patches an existing resource."))

(defgeneric delete-resource (resource)
  (:documentation "This function deletes an existing resource."))

(defmethod view-resource ((resource resource))
  (slot-value resource 'identifier))

(defmethod load-resource ((resource resource)))

(defmethod create-resource ((resource resource) post-data))

(defmethod patch-resource ((resource resource) post-data))

(defmethod delete-resource ((resource resource)))
