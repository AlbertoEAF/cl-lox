(defpackage :lox.class
  (:use :cl :lox-cl :lox.callable)
  (:export :lox-class :make-lox-class))
(in-package :lox.class)

(defclass++ lox-class (lox-callable)
  ((name :type string
         :accessor name)))

(defpackage :lox.instance
  (:use :cl :lox-cl :lox.class)
  (:export :lox-instance :make-lox-instance
           :instance-get :instance-set))
(in-package :lox.instance)


(defclass+ lox-instance ()
  ((lox-class :type lox-class
              :accessor lox-class)
   (fields :type hash-table
           :accessor fields
           :initform (make-hash-table :test #'equal))))

(defun* make-lox-instance ((class lox-class))
  (make-instance 'lox-instance :lox-class class))

;;; Class
(in-package :lox.class)

(defmethod lox-call ((callee lox-class)
                     (interpreter lox.interpreter.def:interpreter)
                     (arguments list))
  (lox.instance:make-lox-instance callee))

(defmethod lox-callable-arity ((callee lox-class))
  0)

(defmethod print-object ((class lox-class) out)
  (format out "<Class ~A>" (name class)))


;;; Instance
(in-package :lox.instance)

(defun* instance-get ((instance lox-instance) (name lox.token:token))
  (multiple-value-bind (value present-p)
      (gethash (lox.token:get-lexeme name)
               (fields instance))
    (if present-p
        value
        (error 'lox.error:lox-runtime-error
               :token name
               :message (format nil "Undefined property '~A'."
                                (lox.token:get-lexeme name))))))

(defun* instance-set ((instance lox-instance) (name lox.token:token) value)
  (setf (gethash (lox.token:get-lexeme name) (fields instance))
        value))

(defmethod print-object ((instance lox-instance) out)
  (format out "<~A instance>" (lox.class::name (lox-class instance))))
