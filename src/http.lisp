(in-package #:restful)


(defun http-error (code)
  (multiple-value-bind (text _)
      (h:reason-phrase (setf (h:return-code*) code))
    (declare (ignore _))
    text))
