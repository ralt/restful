(in-package #:restful)


(defclass resource-metaclass (standard-class) ()
  (:documentation "The metaclass for resources, required to be
used by all the resources.

This metaclass allows resources to use new slot options:

- `is-identifier`: defaults to NIL. Only one slot per resource
should set this option to T. It will make the slot the identifier
of the resource. The identifier is used to find the resource in
the API. When set to T, the slot option `required` is implicitly
set to T too.
- `required`: defaults to NIL. When set to T, this slot will
be required in the API requests.
- `default`: defaults to `\"\"`. If the slot is not required,
this value will be used to fill in the slot value if no value
is provided.
- `excluded`: defaults to NIL. When to set T, this slot will be
ignored for the resource's CRUD actions. For example, the
`restful:resource` class uses it for its `parent` and `storage`
slots."))

(defclass resource-standard-direct-slot-definition (closer-mop:standard-direct-slot-definition)
  ((is-identifier :initarg :is-identifier :initform nil)
   (required :initarg :required :initform nil)
   (default :initarg :default :initform nil)
   (excluded :initarg :excluded :initform nil)))

(defclass resource-standard-effective-slot-definition (closer-mop:standard-effective-slot-definition)
  ((is-identifier :initarg :is-identifier :initform nil)
   (required :initarg :required :initform nil)
   (default :initarg :default :initform nil)
   (excluded :initarg :excluded :initform nil)))

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
        (dolist (sym '(is-identifier required default excluded))
          (setf (slot-value effective-slotd sym) (slot-value slotd sym)))
        (return)))
    effective-slotd))
