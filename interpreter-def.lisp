(defpackage :lox.interpreter.def
  (:use :cl :lox-cl :lox.error)
  (:import-from :lox.environment :environment)
  (:export
   :interpreter :environment :globals :locals
   :evaluate :execute :execute-block
   :lox-return :make-lox-return)
  (:local-nicknames (#:syntax #:lox.syntax)
                    (#:token  #:lox.token)
                    (#:tok-type #:lox.token.types)
                    (#:env      #:lox.environment)))
(in-package :lox.interpreter.def)

;;(named-readtables:in-readtable rutils:rutils-readtable)

#|
Compared to the book:
 - visit<X>Expr() -> evaluate call
 - visit<X>Stmt() -> execute call
|#

(defclass+ interpreter ()
  ((globals :type env:environment
            :reader globals)
   (environment :type env:environment
                :reader environment)
   (locals :type hash-table
           :initform (make-hash-table :test #'eq)
           :reader locals)))

(defgeneric evaluate (interpreter expr)
  (:documentation "For expressions. Equivalent to lox's visit<X>Expr."))

(defgeneric execute (interpreter stmt)
  (:documentation "For statements. Equivalent to lox's visit<X>Stmt"))

(defun* execute-block ((interpreter interpreter) (statements list) (env environment))
  (let ((previous-env (environment interpreter)))
    (setf (slot-value interpreter 'environment)
          env)
    (unwind-protect
         (dolist (statement statements)
           (execute interpreter statement))
      (setf (slot-value interpreter 'environment)
            previous-env))))

(define-condition lox-return (error)
  ((value :initform nil
          :initarg :value))
  (:documentation "Signal that carries the return value information."))

(defun make-lox-return (value)
  "Creates a lox-return condition that transports a value."
  (make-condition 'lox-return :value value))
