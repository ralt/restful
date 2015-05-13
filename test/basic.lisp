(in-package #:restful-test)


(plan 1)

(web-run
  (let ((response (drakma:http-request "http://localhost:4242/foo")))
    (is "[]" response)))

(finalize)
