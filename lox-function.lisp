(defpackage :lox.function
  (:use :cl :defclass+ :lox.interpreter.def :lox.callable :lox.function.def)
  (:export :lox-function :make-lox-function))
(in-package :lox.function)

(named-readtables:in-readtable rutils:rutils-readtable)

(defmethod lox-call ((callee lox-function)
                     (interpreter lox.interpreter.def:interpreter)
                     (arguments list))
  (let ((env (lox.environment:make-environment
              (slot-value interpreter 'lox.interpreter.def::globals))))
    (loop
      for argument in arguments
      for parameter in @callee.declaration.params
      do
         (lox.environment:define env @parameter.lexeme argument))
    (handler-case
        (lox.interpreter.def::execute-block @callee.declaration.body
                                            (lox.interpreter.build::make-proxy-env-interpreter
                                             interpreter env))
      (lox-return (lox-ret)
        (return-from lox-call @lox-ret.value)))
    
    nil))


(defmethod lox-callable-arity ((callee lox-function))
  (length @callee.declaration.params))
