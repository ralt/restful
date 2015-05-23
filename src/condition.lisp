(in-package #:restful)


(define-condition resource-not-found-error (error) ()
  (:documentation "Raised when a request resource is not found.

When this error is handled, the response will have the 404 status
code.

This error can be raised in the following requests: GET, PATCH,
DELETE, POST."))

(define-condition request-data-missing (error) ()
  (:documentation "Raised when a request body doesn't fulfill the
resource's schema.

For example, if a resource is the following:

    (defclass foo (restful:resource)
      ((id :is-identifier t)
       (name :required t))
      (:metaclass restful:resource-metaclass))

And the request body is the following:

    {\"id\":\"bar\"}

This error will be raised.

When this error is handled, the response will have a 400 status
code.

This error can be raised in the following requests: PUT, POST."))

(define-condition permission-rejected (error) ()
  (:documentation "Raised when a request doesn't have access to the
requested resource.

This error is raised when the has-permission method of a resource
returns NIL.

When this error is handled, the response will have the 403 status
code.

This error can be raised with every method."))
