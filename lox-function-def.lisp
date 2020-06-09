(defpackage :lox.function.def
  (:use :cl :defclass+ :lox.callable)
  (:export :lox-function :make-lox-function))
(in-package :lox.function.def)

(named-readtables:in-readtable rutils:rutils-readtable)

(defclass++ lox-function (lox-callable)
  ((declaration :type lox.syntax.stmt:stmt-function)
   (closure :type lox.environment:environment)))
