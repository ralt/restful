(in-package #:restful-test)


(defvar *server* nil)

(defclass foobar (restful:resource)
  ((name :reader name)))
(defclass foobar-collection (restful:collection)
  ())

(defvar *storage* (make-instance 'restful:memory-storage))
(defvar *resource-definition* { "foo" {
        :class 'foobar
        :collection 'foobar-collection
        :storage *storage*} })

(defvar *server* nil)

(defun start-web ()
  (setf *server* (h:start
                  (make-instance 'restful:acceptor
                                 :port 4242
                                 :resource-definition *resource-definition*))))

(defun stop-web ()
  (h:stop *server*))

(defun rest-run (fn)
  (funcall fn "http://localhost:4242"))
