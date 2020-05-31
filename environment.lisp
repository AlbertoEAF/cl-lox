(defpackage :lox.environment
  (:use :cl :defclass+ :defstar :cl-extensions)
  (:export
   :environment
   :make-environment
   :define
   :assign
   :get-value))
(in-package :lox.environment)

(defclass+ environment ()
  ((values :type hash-table
           :initform (make-hash-table :test 'equal))))

(defun make-environment ()
  (make-instance 'environment))

(defun* define ((env environment) (name string) value)
  "Used in variable declarations, to define the variable value."
  (setf (gethash name (slot-value env 'values))
        value))

(defun* assign ((env environment) (name lox.token:token) value)
  "Used to define the value of the variable in a variable declaration."
  (with-slots (values) env
    (let ((name-lexeme (lox.token:get-lexeme name)))
      (multiple-value-bind (old-value present-p) (gethash name-lexeme values)
        (declare (ignore old-value))
        (if present-p (setf (gethash name-lexeme values)
                            value)
            (lox.error:lox-runtime-error
             (make-condition 'lox.error:lox-runtime-error
                             :token name
                             :message (format nil "Undefined variable '~A'." name-lexeme))))))))

(defun* get-value ((environment environment) (name lox.token:token))
  (multiple-value-bind (value present-p)
      (gethash (lox.token:get-lexeme name)
               (slot-value environment 'values))
    (if present-p value
        (lox.error:lox-runtime-error
         (make-condition 'lox.error:lox-runtime-error
                         :token name
                         :message (format nil "Undefined variable '~A'."
                                          (lox.token:get-lexeme name)))))))
