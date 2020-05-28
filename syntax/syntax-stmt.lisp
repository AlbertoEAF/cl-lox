(defpackage :lox.syntax.stmt
  (:use :cl :lox.token :defclass-std :defclass+)
  (:export :stmt
   ;; Statement classes
   :stmt-expression :stmt-print))
   
(in-package :lox.syntax.stmt)
(proclaim '(optimize safety))

(shadowing-import (find-symbol "EXPRESSION" "LOX.SYNTAX.EXPR"))

(defclass+ stmt ()
  ())

(defclass+ stmt-expression (stmt)
  ((expression :type lox.syntax.expr:expr)))

(defclass+ stmt-print (stmt)
  ((expression :type lox.syntax.expr:expr)))
