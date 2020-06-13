(defpackage :lox.class
  (:use :cl :lox-cl :lox.callable)
  (:export :lox-class :make-lox-class :methods :lox-find-method))
(in-package :lox.class)

(defclass++ lox-class (lox-callable)
  ((name :type string
         :accessor name)
   (methods :type hash-table
            :accessor methods)))

(defpackage :lox.instance
  (:use :cl :lox-cl :lox.class)
  (:export :lox-instance :make-lox-instance
           :instance-get :instance-set)
  (:local-nicknames (#:token #:lox.token)))
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

(defun* lox-find-method ((class lox-class) (name string))
  "Returns the method or nil if not defined."
  (multiple-value-bind (value value-p)
      (gethash name (methods class))
    (if value-p value)))

(defmethod lox-call ((class lox-class)
                     (interpreter lox.interpreter.def:interpreter)
                     (arguments list))
  (let ((instance (lox.instance:make-lox-instance class))
        (initializer (lox-find-method class "init")))
    (when initializer
      (lox-call (lox.function:bind initializer instance)
                interpreter
                arguments))
    instance))

(defmethod lox-callable-arity ((class lox-class))
  (let ((initializer (lox-find-method class "init")))
    (if initializer
        (lox-callable-arity initializer)
        0)))

(defmethod print-object ((class lox-class) out)
  (format out "<Class ~A>" (name class)))


;;; Instance
(in-package :lox.instance)

(defun* instance-get ((instance lox-instance) (name token:token))
  (multiple-value-bind (value value-p)
      (gethash (token:get-lexeme name)
               (fields instance))
    (if value-p
        value
        (let ((method (lox-find-method (lox-class instance)
                                       (lox.token:get-lexeme name))))
          (if method
              (lox.function:bind method instance)
              (error 'lox.error:lox-runtime-error
                     :token name
                     :message (format nil "Undefined property '~A'."
                                      (token:get-lexeme name))))))))

(defun* instance-set ((instance lox-instance) (name token:token) value)
  (setf (gethash (token:get-lexeme name) (fields instance))
        value))

(defmethod print-object ((instance lox-instance) out)
  (format out "<~A instance>" (lox.class::name (lox-class instance))))
