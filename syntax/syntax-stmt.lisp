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
   :stmt-block
   :stmt-if))
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

(defclass+ stmt-if (stmt)
  ((condition :type lox.syntax.expr:expr)
   (then-branch :type stmt)
   (else-branch :type (or null stmt))))
