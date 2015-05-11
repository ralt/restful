(in-package #:restful)


(defclass storage () ())

(defgeneric get-item (storage identifier)
  (:documentation "Gets a single item in the storage."))

(defgeneric delete-item (storage identifier)
  (:documentation "Deletes a single item in the storage."))
