(defpackage :cl-extensions
  (:use :cl)
  (:export
   :with-curry
   :let-return-it
   :let-return)
  (:local-nicknames (#:a #:alexandria)))
(in-package :cl-extensions)

;; Credit to coredump: https://stackoverflow.com/questions/61987738/common-lisp-locally-shadow-function-with-same-name/61993302?noredirect=1#comment109662485_61993302
(defmacro with-curry ((&rest fn-specs) prefix &body body)
  "Curry functions with prefix (one or more first arguments)."
  (loop
    with args = (gensym)
    and n = (gensym)
    and prefix = (alexandria:ensure-list prefix)
    for f in fn-specs
    collect (if (and (consp f) (eq 'setf (first f)))
                `(,f (,n &rest ,args) (apply #',f ,n ,@prefix ,args))
                `(,f (&rest ,args) (apply #',f ,@prefix ,args)))
      into flets
    finally (return
              `(flet ,flets
                 (declare (inline ,@fn-specs))
                 ,@body))))


(defmacro let-return-it (it-initform &body body)
  "Similar to let-return but implicitly creates the variable 'it' to assign to."
  `(let ((it ,it-initform))
     ,@body
     it))

(defmacro let-return ((it it-initform) &body body)
  "Similar to let but with a single variable (implying one less level of parenthesis).
   Returns that variable (it) at the end."
  `(let ((,it ,it-initform))
     ,@body
     ,it))
