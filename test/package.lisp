(defpackage #:restful-test
  (:use #:cl #:prove)
  (:local-nicknames (#:h #:hunchentoot)))

(in-package #:restful-test)

;; hash-table literal syntax using braces
(set-macro-character
 #\{
 (lambda (str char)
   (declare (ignore char))
   (let ((*readtable* (copy-readtable *readtable* nil))
         (keep-going t))
     (set-macro-character #\} (lambda (stream char)
                                (declare (ignore char) (ignore stream))
                                (setf keep-going nil)))
     (let ((pairs (loop for key = (read str nil nil t)
                     while keep-going
                     for value = (read str nil nil t)
                     collect (list key value)))
           (retn (gensym)))
       `(let ((,retn (make-hash-table :test #'equal)))
          ,@(mapcar
             (lambda (pair)
               `(setf (gethash ,(car pair) ,retn) ,(cadr pair)))
             pairs)
          ,retn)))))

(defmacro web-run (prefix &body body)
  `(rest-run #'(lambda ,prefix
                 ,@body)))

(defun cat (&rest args)
  (apply #'concatenate 'string args))
