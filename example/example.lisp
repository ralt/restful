(ql:quickload :restful)
(load "hash-table-reader-macro.lisp")

(defclass foobar (restful:resource) ())
(defclass foobar-collection (restful:collection)
  ((name :reader name)))

(let* ((storage (make-instance 'restful:memory-storage))
       (resource-definition { "foo" {:class 'foobar
                            :collection 'foobar-collection :storage storage} }))
  (hunchentoot:start (make-instance 'restful:acceptor
                                    :port 4244
                                    :resource-definition resource-definition)))
