(in-package #:restful)


(define-condition resource-not-found-error (error) ())
(define-condition resource-field-missing (error) ())
(define-condition resource-action-not-allowed (error) ())

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

(defun populate-slot (resource filler)
  (let ()
    (lambda (slot)
      (let ((filler-value))
        (handler-case
            (setf filler-value (getf filler (intern (string-upcase (symbol-name slot))
                                                    :keyword)))
          (simple-type-error ()
            (setf filler-value (slot-default-value resource slot))))
        (if (and (slot-is-required resource slot) (not filler-value))
            (error 'resource-field-missing)
            (setf (slot-value resource slot) filler-value))))))

(defun slot-is-required (resource slot)
  (find-if #'(lambda (slot-definition)
               (when (eq (closer-mop:slot-definition-name slot-definition) slot)
                 (or (slot-value slot-definition 'required)
                     (slot-value slot-definition 'is-identifier))))
           (closer-mop:class-slots (class-of resource))))

(defun slot-default-value (resource slot)
  (let ((slot-definition (find-if
                          #'(lambda (slot-definition)
                              (when (eq (closer-mop:slot-definition-name slot-definition) slot)
                                (slot-value slot-definition 'default)))
                          (closer-mop:class-slots (class-of resource)))))
    (let ((default-value (slot-value slot-definition 'default)))
      (unless default-value
        (error 'resource-field-missing))
      default-value)))

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
