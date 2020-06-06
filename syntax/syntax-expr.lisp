(defpackage :lox.syntax.expr
  (:use :cl :lox.token :defclass-std :defclass+)
  (:export
   ;; Syntax types
   :expr :binary :grouping :literal :unary :var :logical :call
   ;; Constructors
   :make-binary :make-literal :make-grouping :make-unary :make-var :make-assign :make-logical :make-call
   ;; Accessors
   :left :right :operator :expression :value :assign :name))
(in-package :lox.syntax.expr)
(proclaim '(optimize safety))

(defclass expr ()
  ())

(defclass++ assign (expr)
  ((name :type token)
   (value :type expr)))

(defclass++ binary (expr)
  ((left :type expr)
   (operator :type token)
   (right :type expr)))

(defclass++ grouping (expr)
  ((expression :type expr)))

(defclass++ literal (expr)
  ((value)))

(defclass++ unary (expr)
  ((operator :type token)
   (right :type expr)))

(defclass++ var (expr)
  ((name :type token)))

(defclass++ logical (expr)
  ((left :type expr)
   (operator :type token)
   (right :type expr)))

(defclass++ call (expr)
  ((callee :type expr)
   (paren :type token)
   (arguments :type list)))


(defun get-slot-names (clos-obj)
  (mapcar #'sb-mop:slot-definition-name
          (sb-mop:class-direct-slots
           (class-of clos-obj))))

(defmethod print-object ((expr expr) out)
  (print-unreadable-object (expr out :type t :identity t)
    (loop
      for slot-name in (get-slot-names expr)
      for slot-value = (slot-value expr slot-name)
      do (format out " ~A=~A" slot-name slot-value))))
