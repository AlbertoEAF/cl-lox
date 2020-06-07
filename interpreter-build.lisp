(defpackage :lox.interpreter.build
  (:use :cl :lox-cl :lox.interpreter.def)
  (:export :interpreter :make-interpreter :make-proxy-env-interpreter)
  (:local-nicknames (#:syntax #:lox.syntax)
                    (#:token  #:lox.token)
                    (#:tok-type #:lox.token.types)
                    (#:env      #:lox.environment)))
(in-package :lox.interpreter.build)

(named-readtables:in-readtable rutils:rutils-readtable)


(defun make-interpreter ()
  (let* ((globals (env:make-environment))
         (environment (env:make-environment globals)))
    (flet ((define-global-function (name fn &optional (repr (format nil "<native fn ~A>" name)))
             "Helper to define global a function."
             (env:define globals name
               (lox.callable:make-lox-native-function
                name (length (trivial-arguments:arglist fn)) fn repr))))
      (define-global-function "clock" (lambda () (get-universal-time)))
      (define-global-function "readfile" (lambda (fpath) (uiop:read-file-string fpath)))
      (define-global-function "readline" (lambda () (read-line))))
    (make-instance 'interpreter :globals globals :environment environment)))

(defun* make-proxy-env-interpreter ((interpreter interpreter) &optional new-env)
  "Creates an interpreter whose environment is new and points to the input interpreter's environment.

   It shares the globals environment with the input interpreter."
  (make-instance 'interpreter :globals (globals interpreter)
                              :environment (or new-env
                                               (env:make-environment (environment interpreter)))))
