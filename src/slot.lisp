(in-package #:restful)


(defun normalize-keywords (symbols)
  (mapcar #'normalize-keyword symbols))

(defun normalize-keyword (symbol)
  (if (keywordp symbol)
      (intern (string-upcase (symbol-name symbol)) :keyword)
      symbol))

(defun find-identifier-slot (class)
  ;; Dummy instance for closer-mop:class-slots to work
  (make-instance class)
  (closer-mop:slot-definition-name
   (find-if #'(lambda (slot-definition)
                (slot-value slot-definition 'is-identifier))
            (closer-mop:class-slots (find-class class)))))

(defun populate-slot (resource filler)
  (let ()
    (lambda (slot)
      (let ((filler-value (or (getf filler (intern (string-upcase (symbol-name slot))
                                                   :keyword))
                              (slot-default-value resource slot))))
        (if (and (slot-is-required resource slot) (not filler-value))
            (error 'request-data-missing)
            (setf (slot-value resource slot) filler-value))))))

(defun slot-is-required (resource slot)
  (find-if #'(lambda (slot-definition)
               (when (eq (closer-mop:slot-definition-name slot-definition) slot)
                 (or (slot-value slot-definition 'required)
                     (slot-value slot-definition 'is-identifier))))
           (closer-mop:class-slots (class-of resource))))

(defun slot-default-value (resource slot)
  (let ((slot-definition (find-if
                          #'(lambda (slot-definition)
                              (when (eq (closer-mop:slot-definition-name slot-definition) slot)
                                (slot-value slot-definition 'default)))
                          (closer-mop:class-slots (class-of resource)))))
    (unless slot-definition
      (error 'request-data-missing))
    (let ((default-value (slot-value slot-definition 'default)))
      (unless default-value
        (error 'request-data-missing))
      default-value)))

(defun normalize-resource (resource)
  (a:flatten (sort (a:plist-alist resource)
                   #'(lambda (a b)
                       (string< (cdr a) (cdr b))))))

(defun get-resource-slots (resource)
  (mapcar #'closer-mop:slot-definition-name
          (remove-if #'is-slot-excluded
                     (closer-mop:class-slots (class-of resource)))))

(defun is-slot-excluded (slot)
  (slot-value slot 'excluded))
