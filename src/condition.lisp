(in-package #:restful)


(define-condition resource-not-found-error (error) ())
(define-condition request-data-missing (error) ())
(define-condition permission-rejected (error) ())
