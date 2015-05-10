(asdf:defsystem #:restful
  :description "Spin up new REST entities like madman"
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT License"
  :serial t
  :depends-on (:hunchentoot :alexandria :cl-ppcre)
  :components ((:module "src"
                        :components
                        ((:file "package")
                         (:file "resource")
                         (:file "collection")
                         (:file "acceptor")))))
