(in-package #:restful-test)


(defvar *server* nil)

(defclass foobar (restful:resource)
  ((identifier :is-identifier t :reader identifier)
   (name :reader name :required t)
   (foo :reader foo :default "biz")
   (bar :reader bar :excluded t))
  (:metaclass restful::resource-metaclass))
(defclass foobar-collection (restful:collection)
  ())

(defmethod restful:resource-action ((resource foobar))
  (cond ((string= (identifier resource) "login") "login")
        ((string= (identifier resource) "logout") "logout")))

(defvar *server* nil)

(defun start-web (resource-definition)
  (setf *server* (h:start
                  (make-instance 'restful:acceptor
                                 :port 4242
                                 :resource-definition resource-definition))))

(defun stop-web ()
  (h:stop *server*))

(defun rest-run (fn)
  (funcall fn "http://localhost:4242"))
