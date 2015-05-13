(in-package #:restful-test)


(defvar *server* nil)

(defclass foobar (restful:resource) ())
(defclass foobar-collection (restful:collection)
  ((name :reader name)))

(defvar *storage* (make-instance 'restful:memory-storage))
(defvar *resource-definition* { "foo" {
        :class 'foobar
        :collection 'foobar-collection
        :storage *storage*} })

(defun rest-run (fn)
  (let ((server (h:start
                 (make-instance 'restful:acceptor
                                :port 4242
                                :resource-definition *resource-definition*))))
    (funcall fn)
    (h:stop server)))
