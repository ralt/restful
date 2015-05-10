(defpackage #:restful
  (:use #:cl)
  (:local-nicknames (#:h #:hunchentoot)
                    (#:a #:alexandria))
  (:export :acceptor
           :resource
           :collection))
