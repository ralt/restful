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

(defun rest-run (fn)
  (let ((server (h:start
                 (make-instance 'restful:acceptor
                                :port 4242
                                :resource-definition *resource-definition*))))
    (funcall fn "http://localhost:4242")
    (h:stop server)))

(setf restful::*memory-storage-items* (make-hash-table :test #'equal))
