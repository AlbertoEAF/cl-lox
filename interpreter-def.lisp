(defpackage :lox.interpreter.def
  (:use :cl :defstar :defclass+ :lox.error)
  (:export :interpreter :evaluate :execute :execute-block :lox-return :make-lox-return)
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
  ((globals :type env:environment)
   (environment :type env:environment)))

(defgeneric evaluate (interpreter expr)
  (:documentation "For expressions. Equivalent to lox's visit<X>Expr."))

(defgeneric execute (interpreter stmt)
  (:documentation "For statements. Equivalent to lox's visit<X>Stmt"))

(defun* execute-block ((statements list) (interpreter interpreter))
  (loop for statement in statements do (execute interpreter statement)))


(define-condition lox-return (lox-runtime-error)
  ((value :initform nil
          :initarg :value)))

(defun make-lox-return (value)
  (make-condition 'lox-return :value value))
