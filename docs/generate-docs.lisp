#!/usr/bin/sbcl --script

(load "~/.sbclrc")

(ql:quickload :restful)
(ql:quickload :documentation-template)
(ql:quickload :cl-markdown)
(ql:quickload :alexandria)
(documentation-template:create-template
  :restful
  :target "api.html"
  :subtitle "REST APIs made easy"
  :example (multiple-value-bind (_ html)
               (markdown:markdown (alexandria:read-file-into-string "example.md")
                                  :format :html
                                  :stream nil)
             (declare (ignore _))
             html))
