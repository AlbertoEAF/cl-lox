(defpackage :lox.syntax.stmt
  (:use :cl :defclass-std :defclass+)
  (:shadowing-import-from "LOX.SYNTAX.EXPR"
                          "EXPRESSION")
  (:export
   ;; Statement classes
   :stmt
   :stmt-expression
   :stmt-print
   :stmt-var-declaration
   :stmt-block))
(in-package :lox.syntax.stmt)

(defclass+ stmt ()
  ())

(defclass+ stmt-expression (stmt)
  ((expression :type lox.syntax.expr:expr)))

(defclass+ stmt-print (stmt)
  ((expression :type lox.syntax.expr:expr)))

(defclass+ stmt-var-declaration (stmt)
  ((name :type lox.token:token)
   (initializer :type (or null lox.syntax.expr:expr))))

(defclass+ stmt-block (stmt)
  ;; Probably could add checking of individual items at introduction (list stmt*)
  ((statements :type list)))
