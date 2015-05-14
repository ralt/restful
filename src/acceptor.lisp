(in-package #:restful)


(defclass acceptor (h:acceptor)
  ((resource-definition :initarg :resource-definition
                        :type hash-table)))

(defmethod h:acceptor-dispatch-request ((acceptor acceptor) request)
  (handler-case
      (let ((path-parts (mapcar #'string-downcase (rest (cl-ppcre:split "/" (hunchentoot:request-uri request))))))
        (handle-uri path-parts (slot-value acceptor 'resource-definition)))
    (resource-not-found-error ()
      (error-message (setf (h:return-code*) h:+http-not-found+)))))

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
  (let ((resource-instance (make-instance
                            (gethash :class resource-hash-value)
                            :identifier (second parts)
                            :parent parent
                            :storage (gethash :storage resource-hash-value))))
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
          (t (error-message
              (setf (h:return-code*) h:+http-method-not-allowed+))))))

(defun error-message (code)
  (cond ((= code h:+http-not-found+) "Resource not found.")
        ((= code h:+http-method-not-allowed+) "Method not allowed.")))

(defun handle-resource-method (method resource)
  (cond ((eq method :get) (handle-get-resource resource))
        ((eq method :post) (handle-post-resource resource))
        ((eq method :put) (handle-put-resource resource))
        ((eq method :patch) (handle-patch-resource resource))
        ((eq method :delete) (handle-delete-resource resource))
        (t (error-message
            (setf (h:return-code*) h:+http-method-not-allowed+)))))

(defun handle-get-resource (resource)
  (load-resource resource)
  (jonathan:to-json resource))

(defun handle-post-resource (resource)
  ;; NIY
  (declare (ignore resource)))

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
  (delete-resource resource)
  (setf (h:return-code*) h:+http-no-content+) "")

(defun normalize-keywords (symbols)
  (mapcar #'normalize-keyword symbols))

(defun normalize-keyword (symbol)
  (if (keywordp symbol)
      (intern (string-upcase (symbol-name symbol)) :keyword)
      symbol))
