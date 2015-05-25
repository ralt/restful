(in-package #:restful)


(defclass resource ()
  ((parent :initarg :parent :type resource)
   (storage :initarg :storage :type storage :reader storage))
  (:metaclass resource-metaclass)
  (:documentation "Base class for resources. All the resources
should extend this class to have the default (required) slots:

- `parent`: stores the parent of the resource in case of hierarchy.
For example, if hitting foo/bar/baz/qux, the resource with identifier
'qux' will have 'bar' as parent. If there's no parent, it's value
is NIL.
- `storage`: the storage object that satisfies the interface of the
`restful:storage` class."))

(defgeneric view-resource (resource)
  (:documentation "Returns an object that will be serialized
 to json using the jonathan library."))

(defgeneric load-resource (resource)
  (:documentation "Loads a resource based on its identifier."))

(defgeneric replace-resource (resource request-data)
  (:documentation "Replaces a resource based on the request data."))

(defgeneric create-resource (resource request-data)
  (:documentation "Creates a new resource based on the request data."))

(defgeneric patch-resource (resource request-data)
  (:documentation "Patches an existing resource based on the
request data."))

(defgeneric delete-resource (resource)
  (:documentation "Deletes an existing resource."))

(defgeneric resource-action (resource)
  (:documentation "Lets you handle actions on the resource.
The :identifier slot lets you know which action is called. This
method is called for the POST requests, and routing should
be handled by yourself. Here is a typical example of what
it can look like:

    (defmethod resource-action ((res custom-resource))
      (cond ((string= (identifier res) \"login\") #'handle-login)
            (t (http-page-not-found))))"))

(defgeneric has-permission (resource method)
  (:documentation "Determines if the request has permission to
hit the resource. If it doesn't, returns NIL."))

(defmethod jonathan:%to-json ((resource resource))
  "Serializes a resource to json using the jonathan library."
  (jonathan:with-object
    (loop
       :for slot in (get-resource-slots resource)
       :do (jonathan:write-key-value (string-downcase (symbol-name slot))
                                     (slot-value resource slot)))))

(defmethod view-resource ((resource resource))
  "Returns a plist representing the resource."
  (let ((slots (get-resource-slots resource)))
    (a:flatten
     (mapcar #'(lambda (slot)
                 (list (intern (string-upcase (symbol-name slot)) :keyword)
                       (slot-value resource slot)))
             slots))))

(defmethod load-resource ((resource resource))
  "Loads the resource using its storage. A resource-not-found-error
error is raised if the resource was not found."
  (let ((item (get-item (storage resource) (slot-value resource
                                                       (find-identifier-slot
                                                        (class-name
                                                         (class-of resource)))))))
    (if item
        (populate-resource resource item)
        (error 'resource-not-found-error))))

(defmethod replace-resource ((resource resource) request-data)
  "Replaces the resources based on the request data."
  (populate-resource resource request-data)
  (save-item (storage resource) resource))

(defmethod create-resource ((resource resource) request-data)
  "Creates a new resource in the storage based on the request data."
  (populate-resource resource request-data)
  (save-item (storage resource) resource))

(defmethod patch-resource ((resource resource) request-data)
  "Patches an existing resource based on the request data."
  (loop
     :for slot in (get-resource-slots resource)
     :do (let ((slot-keyword (intern (string-upcase (symbol-name slot)) :keyword)))
           (when (member slot-keyword request-data)
             (setf (slot-value resource slot) (getf request-data slot-keyword)))))
  (save-item (storage resource) resource))

(defmethod resource-action ((resource resource))
  "Returns a 404 Page Not Found. Raising a resource-not-found error
doesn't make sense."
  (http-error h:+http-not-found+))

(defmethod delete-resource ((resource resource))
  "Deletes an existing resource. If the resource doesn't exist,
an error was already thrown earlier thanks to load-resource."
  (delete-item (storage resource) (slot-value resource (find-identifier-slot
                                                        (class-name
                                                         (class-of resource))))))

(defmethod has-permission ((resource resource) method)
  "Returns T. Override this method to change the behavior."
  (declare (ignore resource method))
  t)

(defun populate-resource (resource filler)
  (let ((slots (get-resource-slots resource)))
    (mapcar (populate-slot resource filler) slots)))

(defun equal-resource (resource1 resource2)
  (equal (normalize-resource resource1)
         (normalize-resource resource2)))
