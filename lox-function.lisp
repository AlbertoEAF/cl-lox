(defpackage :lox.function
  (:use :cl :defclass+ :lox.interpreter.def :lox.callable :lox.function.def)
  (:export :lox-function :make-lox-function)
  (:local-nicknames (#:env #:lox.environment)
                    (#:ℑ.def #:lox.interpreter.def)
                    (#:ℑ.build #:lox.interpreter.build)))
(in-package :lox.function)

(named-readtables:in-readtable rutils:rutils-readtable)

(defmethod lox-call ((callee lox-function)
                     (interpreter ℑ.def:interpreter)
                     (arguments list))
  (let* ((call-env (env:make-environment @callee.closure))
         (call-scope-interpreter (ℑ.build:make-proxy-env-interpreter interpreter call-env))
         (declaration @callee.declaration))
    (loop
      for argument in arguments
      for parameter in @declaration.params
      do (env:define (environment call-scope-interpreter) @parameter.lexeme argument))
    (handler-case
        (ℑ.def:execute-block @declaration.body call-scope-interpreter)
      (lox-return (lox-ret)
        (return-from lox-call @lox-ret.value)))
    nil))

(defmethod lox-callable-arity ((callee lox-function))
  (length @callee.declaration.params))

(defmethod print-object ((lox-function lox-function) out)
  (with-slots (declaration) lox-function
    (format out "<fn ~A~A>"
            @declaration.name.lexeme
            (format nil "(~{~A~^, ~})" (mapcar #'lox.token:get-lexeme @declaration.params)))))
