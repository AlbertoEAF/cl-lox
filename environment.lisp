(defpackage :lox.environment
  (:use :cl :defclass+ :defstar)
  (:export
   :environment
   :make-environment
   :define))
(in-package :lox.environment)

(defclass+ environment ()
  ((values :type hash-table
           :initform (make-hash-table :test 'equal))))

(defun make-environment ()
  (make-instance 'environment))

(defun* define ((environment environment) (name string) value)
  (setf (gethash name (slot-value environment 'values))
        value))

(defun* get-value ((environment environment) (name lox.token:token))
  (multiple-value-bind (value present-p)
      (gethash (lox.token:get-lexeme name)
               (slot-value environment 'values))
    (if present-p value
        (error 'lox.error:lox-runtime-error
               :token name
               :message (format nil "Undefined variable '~A'."
                                (lox.token:get-lexeme name))))))
