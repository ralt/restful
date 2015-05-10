(in-package #:restful)


(defclass acceptor (h:acceptor)
  ((resource :initarg :resource
             :type hash-table)))

(defmethod h:acceptor-dispatch-request ((acceptor acceptor) request)
  (let ((path-parts (mapcar #'string-downcase (rest (cl-ppcre:split "/" (hunchentoot:request-uri request))))))
    (handle-uri path-parts (slot-value acceptor 'resource))))

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
        (error-message
         (setf (h:return-code*) h:+http-not-found+)))))

(defun handle-resource (parts resource-hash-value parent)
  (let ((resource-instance (make-instance
                            (gethash :class resource-hash-value)
                            :identifier (second parts)
                            :parent parent)))
    (if (rest (rest parts))
        (handle-uri (rest (rest parts))
                    (gethash :children resource-hash-value)
                    resource-instance)
        (jonathan:to-json (view-document resource-instance)))))

(defun handle-collection (resource-hash-value parent)
  (jonathan:to-json
   (view-collection
    (make-instance (gethash :collection resource-hash-value)
                   :parent parent))))

(defun error-message (code)
  (cond ((= code h:+http-not-found+) "Resource not found")))
