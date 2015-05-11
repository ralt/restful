(defpackage #:restful
  (:use #:cl)
  (:local-nicknames (#:h #:hunchentoot)
                    (#:a #:alexandria))
  (:export
   ;; classes
   :acceptor
   :resource
   :collection
   :storage
   :memory-storage

   ;; resource's methods
   :view-resource
   :load-resource
   :replace-resource
   :create-resource
   :patch-resource
   :delete-resource

   ;; collection's methods
   :view-collection

   ;; storage's methods
   :get-item
   :delete-item))
