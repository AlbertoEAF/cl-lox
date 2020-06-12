(defpackage :lox.function
  (:use :cl :lox-cl :lox.interpreter.def :lox.callable :lox.function.def)
  (:export :lox-function :make-lox-function :bind)
  (:local-nicknames (#:env #:lox.environment)
                    (#:ℑ.def #:lox.interpreter.def)
                    (#:ℑ.build #:lox.interpreter.build)))
(in-package :lox.function)

(named-readtables:in-readtable rutils:rutils-readtable)

(defmethod lox-call ((callee lox-function)
                     (interpreter ℑ.def:interpreter)
                     (arguments list))
  (let ((call-env (env:make-environment @callee.closure))
        (declaration @callee.declaration))
    (loop
      for argument in arguments
      for parameter in @declaration.params
      do (env:define call-env @parameter.lexeme argument))
    (handler-case
        (ℑ.def:execute-block interpreter @declaration.body call-env)
      (lox-return (lox-ret)
        (return-from lox-call @lox-ret.value)))
    nil))

(defmethod lox-callable-arity ((callee lox-function))
  (length @callee.declaration.params))

(defun* bind ((method lox-function) instance)
  "Instance is of type lox.instance:lox-instance."
  (let ((env (env:make-environment @method.closure)))
    (env:define env "this" instance)
    (make-lox-function @method.declaration env)))

(defmethod print-object ((lox-function lox-function) out)
  (with-slots (declaration) lox-function
    (format out "<fn ~A~A>"
            @declaration.name.lexeme
            (format nil "(~{~A~^, ~})" (mapcar #'lox.token:get-lexeme @declaration.params)))))
