(in-package #:restful)


(defclass acceptor (h:acceptor)
  ((resource-definition :initarg :resource-definition
                        :type hash-table))
  (:documentation "Base class for the acceptor, subclassing
a hunchentoot acceptor to be able to handle incoming requests.

This class defines the following slot:

- `resource-definition`: defines the list of resources and
how to handle them. This is a hash table that must:
    - define a string key being the prefix for the resources
    - for each key, define a value being a hash table. This
      hash table must:
        - define a keyword key named `:class` being the resource
          class.
        - define a keyword named `:collection` being the collection
          class.
        - define a keyword named `:storage` being the storage instance.
        - define a keyword named `:children` being a new resource
          definition, if necessary.

This is an example of a resource definition (assuming the readers
macros to define hash tables using brackets):

    {
        \"article\" {
            :class 'article
            :collection 'restful:collection
            :storage (make-instance 'restful:memory-storage)
            :children {
                \"comment\" {
                    :class 'comment
                    :collection 'restful:collection
                    :storage (make-instance 'restful:memory-storage)
                }
            }
        }
    }

The API will be available through the following endpoints:

- `/article`: collection endpoint.
- `/article/foo`: resource 'foo' of instance `article` endpoint.
- `/article/foo/comment/bar`: resource 'bar' of instance `comment`,
having for parent 'foo' of instance `article` endpoint.

This class should be used to instantiate objects to be used
with hunchentoot.

Here is an example of such usage:

    (hunchentoot:start
      (make-instance 'restful:acceptor
                     :port 4242
                     :resource-definition *resource-definition*))"))

(defmethod h:acceptor-dispatch-request ((acceptor acceptor) request)
  "Dispatches requests to the internal handle-uri function if the
request matches the requirements. Namely, accepting application/json.

This method also defines the application/json response header.

This method catches the following errors:

- `resource-not-found-error`: returns a 404 page not found response.
- `request-data-missing`: returns a 400 bad request response.
- `permission-rejected`: returns a 403 forbidden response.
- `error`: returns a 500 internal server error response."
  ;; Only application/json
  (unless (cl-ppcre:scan "application/json" (h:header-in* :accept))
    (return-from h:acceptor-dispatch-request
      (http-error h:+http-not-acceptable+)))
  (setf (h:header-out :content-type) "application/json; charset=UTF-8")
  (handler-case
      (let ((path-parts (mapcar #'string-downcase (rest (cl-ppcre:split "/" (hunchentoot:request-uri request))))))
        (handle-uri path-parts (slot-value acceptor 'resource-definition)))
    (resource-not-found-error ()
      (http-error h:+http-not-found+))
    (request-data-missing ()
      (http-error h:+http-bad-request+))
    (permission-rejected ()
      (http-error h:+http-forbidden+))
    (error ()
      (http-error h:+http-internal-server-error+))))

(defun handle-uri (parts resources &optional parent)
  (let* ((keys (mapcar #'string-downcase (a:hash-table-keys resources)))
         (resource-name (find (first parts) keys :test #'string=)))
    (if resource-name
        (if (rest parts)
            (handle-resource parts
                             (gethash resource-name resources)
                             parent)
            (handle-collection (gethash resource-name resources)
                               parent))
        (error 'resource-not-found-error))))

(defun handle-resource (parts resource-hash-value parent)
  (let* ((identifier-slot (find-identifier-slot (gethash :class resource-hash-value)))
         (resource-instance (make-instance
                             (gethash :class resource-hash-value)
                             :parent parent
                             :storage (gethash :storage resource-hash-value))))
    (setf (slot-value resource-instance identifier-slot) (second parts))
    (if (rest (rest parts))
        (handle-uri (rest (rest parts))
                    (gethash :children resource-hash-value)
                    resource-instance)
        (handle-resource-method (h:request-method*) resource-instance))))

(defun handle-collection (resource-hash-value parent)
  (let ((method (h:request-method*)))
    (cond ((eq method :get)
           (jonathan:to-json
            (view-collection
             (make-instance (gethash :collection resource-hash-value)
                            :parent parent
                            :storage (gethash :storage resource-hash-value)
                            :class-of-resource (gethash :class
                                                        resource-hash-value)))))
          (t (http-error h:+http-method-not-allowed+)))))

(defun handle-resource-method (method resource)
  (unless (has-permission resource method)
    (error 'permission-rejected))
  (cond ((eq method :get) (handle-get-resource resource))
        ((eq method :post) (handle-post-resource resource))
        ((eq method :put) (handle-put-resource resource))
        ((eq method :patch) (handle-patch-resource resource))
        ((eq method :delete) (handle-delete-resource resource))
        (t (http-error h:+http-method-not-allowed+))))

(defun handle-get-resource (resource)
  (load-resource resource)
  (jonathan:to-json resource))

(defun handle-post-resource (resource)
  (resource-action resource))

(defun handle-put-resource (resource)
  (handler-case
      (progn
        (load-resource resource)
        (let ((post-data (normalize-keywords
                          (jonathan:parse (h:raw-post-data :force-text t)))))
          (unless (equal-resource (view-resource resource) post-data)
            (replace-resource resource post-data))
          (setf (h:return-code*) h:+http-no-content+) ""))
    (resource-not-found-error ()
      (create-resource resource
                       (normalize-keywords
                        (jonathan:parse (h:raw-post-data :force-text t))))
      (setf (h:return-code*) h:+http-created+) "")))

(defun handle-patch-resource (resource)
  (load-resource resource)
  (patch-resource resource (normalize-keywords
                            (jonathan:parse (h:raw-post-data :force-text t))))
  (setf (h:return-code*) h:+http-no-content+) "")

(defun handle-delete-resource (resource)
  (load-resource resource)
  (delete-resource resource)
  (setf (h:return-code*) h:+http-no-content+) "")
