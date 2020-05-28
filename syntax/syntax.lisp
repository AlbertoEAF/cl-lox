(defpackage :lox.syntax
  (:use :cl :lox.token :defclass-std :defclass+))
(in-package :lox.syntax)
(proclaim '(optimize safety))

(do-external-symbols (s "LOX.SYNTAX.EXPR")
  (import s)
  (export s))

(do-external-symbols (s "LOX.SYNTAX.STMT")
  (import s)
  (export s))
