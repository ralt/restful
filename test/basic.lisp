(in-package #:restful-test)


(plan 4)

(web-run (prefix)
  (let ((response (drakma:http-request (cat prefix "/foo"))))
    (is "[]" response)))

(web-run (prefix)
  (multiple-value-bind (_ status-code)
      (drakma:http-request (cat prefix "/foo/bar")
                           :method :put
                           :content "{\"identifier\":\"bar\",\"name\":\"qux\"}")
    (declare (ignore _))
    (is status-code 201)))

(web-run (prefix)
  (multiple-value-bind (_ status-code)
      (drakma:http-request (cat prefix "/foo/bar")
                           :method :put
                           :content "{\"identifier\":\"bar\",\"name\":\"qux\"}")
    (declare (ignore _))
    (is status-code 204)))

(web-run (prefix)
  (let ((response (drakma:http-request (cat prefix "/foo/bar"))))
    (is response "{\"identifier\":\"bar\",\"name\":\"qux\"}")))

(finalize)
