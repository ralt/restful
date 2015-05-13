(in-package #:restful)


(define-condition resource-not-found-error (error) ())

(defclass resource ()
  ((identifier :initarg :identifier :reader identifier)
   (parent :initarg :parent :type resource)
   (storage :initarg :storage :type storage :reader storage)))

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

(defmethod view-resource ((resource resource))
  (let ((slots (get-resource-slots resource)))
    (a:flatten
     (mapcar #'(lambda (slot)
                 (list (intern (symbol-name slot) :keyword)
                       (slot-value resource slot)))
             slots))))

(defmethod load-resource ((resource resource))
  (let ((item (get-item (storage resource) (identifier resource))))
    (if item
        (populate-resource resource item)
        (error 'resource-not-found-error))))

(defmethod replace-resource ((resource resource) post-data)
  (populate-resource resource post-data)
  (save-item (storage resource) resource))

(defmethod create-resource ((resource resource) post-data)
  (populate-resource resource post-data)
  (save-item (storage resource) resource))

(defmethod patch-resource ((resource resource) post-data))

(defmethod delete-resource ((resource resource))
  (delete-item (storage resource) (identifier resource)))

(defun populate-resource (resource filler)
  (let ((slots (get-resource-slots resource)))
    (mapcar #'(lambda (slot)
                (setf (slot-value resource slot)
                      (getf filler (intern (string-downcase (symbol-name slot))
                                           :keyword))))
            slots)))

(defun equal-resource (resource1 resource2)
  (equal (normalize-resource resource1)
         (normalize-resource resource2)))

(defun normalize-resource (resource)
  (a:flatten (sort (a:plist-alist resource)
                   #'(lambda (a b)
                       (string< (cdr a) (cdr b))))))

(defun get-resource-slots (resource)
  (remove-if #'(lambda (slot)
                 (or (eq slot 'parent)
                     (eq slot 'storage)))
             (mapcar #'closer-mop:slot-definition-name
                     (closer-mop:class-slots (class-of resource)))))
