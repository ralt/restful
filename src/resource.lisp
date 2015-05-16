(in-package #:restful)


(defclass resource ()
  ((parent :initarg :parent :type resource)
   (storage :initarg :storage :type storage :reader storage))
  (:metaclass resource-metaclass))

(defgeneric view-resource (resource)
  (:documentation "This function should return an object that will be
serialized to json using the jonathan library."))

(defgeneric load-resource (resource)
  (:documentation "This function loads a resource based on its identifier."))

(defgeneric replace-resource (resource post-data)
  (:documentation "This function replaces a resource."))

(defgeneric create-resource (resource post-data)
  (:documentation "This function creates a new resource."))

(defgeneric patch-resource (resource post-data)
  (:documentation "This function patches an existing resource."))

(defgeneric delete-resource (resource)
  (:documentation "This function deletes an existing resource."))

(defgeneric has-permission (resource method)
  (:documentation "This function determines if the request has permission to
hit the resource."))

(defmethod jonathan:%to-json ((resource resource))
  (jonathan:with-object
    (loop
       :for slot in (get-resource-slots resource)
       :do (jonathan:write-key-value (string-downcase (symbol-name slot))
                                     (slot-value resource slot)))))

(defmethod view-resource ((resource resource))
  (let ((slots (get-resource-slots resource)))
    (a:flatten
     (mapcar #'(lambda (slot)
                 (list (intern (string-upcase (symbol-name slot)) :keyword)
                       (slot-value resource slot)))
             slots))))

(defmethod load-resource ((resource resource))
  (let ((item (get-item (storage resource) (slot-value resource
                                                       (find-identifier-slot
                                                        (class-name
                                                         (class-of resource)))))))
    (if item
        (populate-resource resource item)
        (error 'resource-not-found-error))))

(defmethod replace-resource ((resource resource) post-data)
  (populate-resource resource post-data)
  (save-item (storage resource) resource))

(defmethod create-resource ((resource resource) post-data)
  (populate-resource resource post-data)
  (save-item (storage resource) resource))

(defmethod patch-resource ((resource resource) post-data)
  (loop
     :for slot in (get-resource-slots resource)
     :do (let ((slot-keyword (intern (string-upcase (symbol-name slot)) :keyword)))
           (when (member slot-keyword post-data)
             (setf (slot-value resource slot) (getf post-data slot-keyword)))))
  (save-item (storage resource) resource))

(defmethod delete-resource ((resource resource))
  (delete-item (storage resource) (slot-value resource (find-identifier-slot
                                                        (class-name
                                                         (class-of resource))))))

(defmethod has-permission ((resource resource) method)
  (declare (ignore resource method))
  t)

(defun populate-resource (resource filler)
  (let ((slots (get-resource-slots resource)))
    (mapcar (populate-slot resource filler) slots)))

(defun equal-resource (resource1 resource2)
  (equal (normalize-resource resource1)
         (normalize-resource resource2)))
