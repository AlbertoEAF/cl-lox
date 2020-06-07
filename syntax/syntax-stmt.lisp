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
   :stmt-if
   :stmt-while
   :stmt-function
   :stmt-return
   ;; Constructors
   :make-stmt-expression
   :make-stmt-block
   :make-stmt-while
   :make-stmt-function
   :make-stmt-return))
(in-package :lox.syntax.stmt)

(defclass+ stmt ()
  ())

(defclass++ stmt-expression (stmt)
  ((expression :type lox.syntax.expr:expr)))

(defclass+ stmt-print (stmt)
  ((expression :type lox.syntax.expr:expr)))

(defclass+ stmt-var-declaration (stmt)
  ((name :type lox.token:token)
   (initializer :type (or null lox.syntax.expr:expr))))

(defclass++ stmt-block (stmt)
  ;; Probably could add checking of individual items at introduction (list stmt*)
  ((statements :type list)))

(defclass+ stmt-if (stmt)
  ((condition :type lox.syntax.expr:expr)
   (then-branch :type stmt)
   (else-branch :type (or null stmt))))

(defclass++ stmt-while (stmt)
  ((condition :type lox.syntax.expr:expr)
   (body :type stmt)))

(defclass++ stmt-function (stmt)
  ((name :type lox.token:token)
   (params :type list)
   (body :type list)))

(defclass++ stmt-return (stmt)
  ((keyword :type lox.token:token)
   (value :type lox.syntax.expr:expr)))
