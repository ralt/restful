(in-package #:restful-test)


(plan 1)

(web-run (prefix)
  (let ((response (drakma:http-request (cat prefix "/foo"))))
    (is "[]" response)))

(finalize)
