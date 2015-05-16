(in-package #:restful-test)


(plan 15)

(start-web)

(setf restful::*memory-storage-items* (make-hash-table :test #'equal))
(setf drakma:*text-content-types* '(("application" . "json")))

(web-run (prefix)
  (let ((response (drakma:http-request (cat prefix "/foo")
                                       :accept "application/json")))
    (is response "[]")))

(web-run (prefix)
  (multiple-value-bind (_ status-code)
      (drakma:http-request (cat prefix "/foo/bar")
                           :method :put
                           :content "{\"identifier\":\"bar\",\"name\":\"qux\"}"
                           :accept "application/json")
    (declare (ignore _))
    (is status-code 201)))

(web-run (prefix)
  (multiple-value-bind (_ status-code)
      (drakma:http-request (cat prefix "/foo/bar")
                           :method :put
                           :content "{\"identifier\":\"bar\",\"name\":\"qux\"}"
                           :accept "application/json")
    (declare (ignore _))
    (is status-code 204)))

(web-run (prefix)
  (let ((response (drakma:http-request (cat prefix "/foo/bar")
                                       :accept "application/json")))
    (is response "{\"identifier\":\"bar\",\"name\":\"qux\",\"foo\":\"biz\"}")))

(web-run (prefix)
  (let ((response (drakma:http-request (cat prefix "/foo")
                                       :accept "application/json")))
    (is response "[{\"identifier\":\"bar\",\"name\":\"qux\",\"foo\":\"biz\"}]")))

(web-run (prefix)
  (multiple-value-bind (_ status-code)
      (drakma:http-request (cat prefix "/foo/baz")
                           :method :put
                           :content "{\"identifier\":\"baz\",\"name\":\"qux\"}"
                           :accept "application/json")
    (declare (ignore _))
    (is status-code 201)))

(web-run (prefix)
  (let ((response (drakma:http-request (cat prefix "/foo")
                                       :accept "application/json")))
    (is response "[{\"identifier\":\"baz\",\"name\":\"qux\",\"foo\":\"biz\"},{\"identifier\":\"bar\",\"name\":\"qux\",\"foo\":\"biz\"}]")))

(web-run (prefix)
  (multiple-value-bind (_ status-code)
      (drakma:http-request (cat prefix "/foo/baz")
                           :method :delete
                           :accept "application/json")
    (declare (ignore _))
    (is status-code 204)))

(web-run (prefix)
  (let ((response (drakma:http-request (cat prefix "/foo")
                                       :accept "application/json")))
    (is response "[{\"identifier\":\"bar\",\"name\":\"qux\",\"foo\":\"biz\"}]")))

(web-run (prefix)
  (multiple-value-bind (_ status-code)
      (drakma:http-request (cat prefix "/foo/baz")
                           :method :delete
                           :accept "application/json")
    (declare (ignore _))
    (is status-code 404)))

(web-run (prefix)
  (multiple-value-bind (_ status-code)
      (drakma:http-request (cat prefix "/foo/bar")
                           :method :patch
                           :content "{\"name\":\"biz\"}"
                           :accept "application/json")
    (declare (ignore _))
    (is status-code 204)))

(web-run (prefix)
  (let ((response (drakma:http-request (cat prefix "/foo/bar")
                                       :accept "application/json")))
    (is response "{\"identifier\":\"bar\",\"name\":\"biz\",\"foo\":\"biz\"}")))

(web-run (prefix)
  (multiple-value-bind (_ status-code headers)
      (drakma:http-request (cat prefix "/foo") :accept "application/json")
    (declare (ignore _))
    (is status-code 200)
    (is (drakma:header-value :content-type headers) "application/json; charset=UTF-8")))

(web-run (prefix)
  (multiple-value-bind (_ status-code)
      (drakma:http-request (cat prefix "/foo/qux")
                           :method :put
                           :accept "application/json"
                           :content "{\"identifier\":\"qux\"}")
    (declare (ignore _))
    (is status-code 400)))

(stop-web)

(finalize)
