(in-package #:restful-test)


(plan 10)

(start-web)

(setf restful::*memory-storage-items* (make-hash-table :test #'equal))

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

(web-run (prefix)
  (let ((response (drakma:http-request (cat prefix "/foo"))))
    (is response "[{\"identifier\":\"bar\",\"name\":\"qux\"}]")))

(web-run (prefix)
  (multiple-value-bind (_ status-code)
      (drakma:http-request (cat prefix "/foo/baz")
                           :method :put
                           :content "{\"identifier\":\"baz\",\"name\":\"qux\"}")
    (declare (ignore _))
    (is status-code 201)))

(web-run (prefix)
  (let ((response (drakma:http-request (cat prefix "/foo"))))
    (is response "[{\"identifier\":\"baz\",\"name\":\"qux\"},{\"identifier\":\"bar\",\"name\":\"qux\"}]")))

(web-run (prefix)
  (multiple-value-bind (_ status-code)
      (drakma:http-request (cat prefix "/foo/baz")
                           :method :delete)
    (declare (ignore _))
    (is status-code 204)))

(web-run (prefix)
  (let ((response (drakma:http-request (cat prefix "/foo"))))
    (is response "[{\"identifier\":\"bar\",\"name\":\"qux\"}]")))

(web-run (prefix)
  (multiple-value-bind (_ status-code)
      (drakma:http-request (cat prefix "/foo/baz")
                           :method :delete)
    (declare (ignore _))
    (is status-code 404)))

(stop-web)

(finalize)
