(defpackage #:restful
  (:use #:cl)
  (:local-nicknames (#:h #:hunchentoot)
                    (#:a #:alexandria))
  (:export
   ;; classes
   :acceptor
   :resource
   :resource-metaclass
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
   :has-permission
   :resource-action
   :parent
   :storage

   ;; collection's methods
   :view-collection

   ;; storage's methods
   :get-items
   :get-item
   :delete-item
   :save-item))
