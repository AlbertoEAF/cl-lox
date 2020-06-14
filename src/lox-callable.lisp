(defpackage :lox.callable
  (:use :cl :defclass+)
  (:export
   :lox-callable
   :lox-call
   :lox-callable-arity
   :make-lox-native-function))
(in-package :lox.callable)

(defclass lox-callable () ())

(defgeneric lox-callable-arity (callee)
  (:documentation "Returns the arity of the callable."))

(defgeneric lox-call (callable interpreter arguments)
  (:documentation "Generic method to call the callable."))

;;; Lox native functions

(defclass++ lox-native-function (lox-callable)
  ;; "For native functions like clock."
  ((name :type string)
   (arity :type integer)
   (fn :type function)
   (str-repr :type string)))

(defmethod lox-callable-arity ((callee lox-native-function))
  (slot-value callee 'arity))

(defmethod lox-call ((callee lox-native-function)
                     (interpreter lox.interpreter.def:interpreter)
                     (arguments list))
  (apply (slot-value callee 'fn) arguments))


