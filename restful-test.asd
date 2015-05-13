(asdf:defsystem #:restful-test
  :description "Tests for restful"
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT License"
  :serial t
  :depends-on (:restful :prove :drakma)
  :defsystem-depends-on (:prove-asdf)
  :components ((:module "test"
                        :components
                        ((:file "package")
                         (:file "setup")
                         (:test-file "basic"))))
  :perform (asdf:test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
