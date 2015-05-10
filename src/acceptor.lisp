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
            (let ((resource-instance (make-instance
                                      (gethash :class (gethash resource-name resources))
                                      :identifier (second parts)
                                      :parent parent)))
              (if (rest (rest parts))
                  (handle-uri (rest (rest parts))
                              (gethash :children (gethash resource-name resources))
                              resource-instance)
                  (jonathan:to-json (view-document resource-instance))))
            (jonathan:to-json
             (view-collection
              (make-instance (gethash :collection
                                      (gethash resource-name resources))))))
        (setf (h:return-code*) h:+http-not-found+))))
