(in-package #:restful)


(defclass resource-metaclass (standard-class) ())

(defclass resource-standard-direct-slot-definition (closer-mop:standard-direct-slot-definition)
  ((is-identifier :initarg :is-identifier :initform nil)
   (required :initarg :required :initform nil)))

(defclass resource-standard-effective-slot-definition (closer-mop:standard-effective-slot-definition)
  ((is-identifier :initarg :is-identifier :initform nil)
   (required :initarg :required :initform nil)))

(defmethod closer-mop:direct-slot-definition-class ((class resource-metaclass) &rest initargs)
  (declare (ignore initargs))
  (find-class 'resource-standard-direct-slot-definition))

(defmethod closer-mop:effective-slot-definition-class ((class resource-metaclass) &rest initargs)
  (declare (ignore initargs))
  (find-class 'resource-standard-effective-slot-definition))

(defmethod closer-mop:validate-superclass ((class resource-metaclass) (superclass standard-class))
  t)

(defmethod closer-mop:compute-effective-slot-definition
    ((class resource-metaclass)
     name
     direct-slot-definitions)
  ;; Copy the function into the effective slot definition
  ;; if appropriate.
  (let ((effective-slotd (call-next-method)))
    (dolist (slotd direct-slot-definitions)
      (when (typep slotd 'resource-standard-direct-slot-definition)
        (setf
         (slot-value effective-slotd 'is-identifier) (slot-value slotd 'is-identifier)
         (slot-value effective-slotd 'required) (slot-value slotd 'required))
        (return)))
    effective-slotd))
