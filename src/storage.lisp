(in-package #:restful)


(defclass storage () ())

(defgeneric get-item (storage identifier)
  (:documentation "Gets a single item in the storage."))

(defgeneric delete-item (storage identifier)
  (:documentation "Deletes a single item in the storage."))

(defgeneric save-item (storage resource)
  (:documentation "Saves an item in the storage, creating it if needed."))
