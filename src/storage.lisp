(in-package #:restful)


(defgeneric get-items (storage)
  (:documentation "Gets all the items available in the storage.

This should be overridden for a specific storage if sorting/filtering
options want to be added.

Returns a list of plists."))

(defgeneric get-item (storage identifier)
  (:documentation "Gets a single item in the storage. Returns a plist."))

(defgeneric delete-item (storage identifier)
  (:documentation "Deletes a single item in the storage."))

(defgeneric save-item (storage resource)
  (:documentation "Saves an item in the storage, creating it if needed.

Since resources are created or updated with PUT requests, as long as you
have an ID, a resource exists. Except if the user doesn't have permission
to PUT non-existing resources, but that's handled at the application level,
not at the storage level, which only cares that a resource has an identifier."))
