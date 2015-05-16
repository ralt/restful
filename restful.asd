(asdf:defsystem #:restful
  :description "Spin up new REST entities like madman"
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT License"
  :serial t
  :depends-on (:hunchentoot :alexandria :cl-ppcre :jonathan :closer-mop)
  :in-order-to ((asdf:test-op (asdf:test-op #:restful-test)))
  :components ((:module "src"
                        :components
                        ((:file "package")
                         (:file "condition")
                         (:file "storage")
                         (:file "storages/memory")
                         (:file "resource-metaclass")
                         (:file "slot")
                         (:file "resource")
                         (:file "collection")
                         (:file "http")
                         (:file "acceptor")))))
