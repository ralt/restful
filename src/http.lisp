(in-package #:restful)


(defun error-message (code)
  (cond
    ((= code h:+http-bad-request+) "Bad request.")
    ((= code h:+http-forbidden+) "Forbidden.")
    ((= code h:+http-not-found+) "Resource not found.")
    ((= code h:+http-method-not-allowed+) "Method not allowed.")
    ((= code h:+http-internal-server-error+) "Internal server error.")))

(defun http-error (code)
  (error-message (setf (h:return-code*) code)))
