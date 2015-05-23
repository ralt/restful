(in-package #:restful)


(defclass memory-storage ()
  ((storage :reader storage :initform (make-hash-table :test 'equal)
            :documentation "The hash table used to store the items
in memory."))
  (:documentation "Restful's storage by storing the data in memory.

This is of course not very good for persistence matters (duh), but is
perfect for development, and simply to show how a storage class
can be built.

This storage just uses a hash table stored in a slot."))

(defmethod get-items ((storage memory-storage))
  "Gets all the items stored in the current instance."
  (a:hash-table-values (storage storage)))

(defmethod get-item ((storage memory-storage) identifier)
  "Gets a single item according to its identifier. Returns
a plist. The identifier is the key of the hash table."
  (gethash identifier (storage storage)))

(defmethod delete-item ((storage memory-storage) identifier)
  "Deletes the item from the hash table. The identifier
is the key of the hash table."
  (remhash identifier (storage storage)))

(defmethod save-item ((storage memory-storage) resource)
  "Saves or updates an item in the hash table. Since
`(setf (gethash [...]))` is used, the 'save or update'
feature is very simply done."
  (let ((result (view-resource resource)))
    (setf (gethash (getf
                    result
                    (normalize-keyword
                     (intern
                      (symbol-name
                       (find-identifier-slot
                        (class-name (class-of resource))))
                      :keyword)))
                   (storage storage))
          result)))
