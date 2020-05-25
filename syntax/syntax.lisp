(defpackage :lox.syntax
  (:use :cl :lox.token :defclass-std :defclass+)
  (:export
   ;; Syntax types
   :expr :binary :grouping :literal :unary
   ;; Constructors
   :make-binary :make-literal
   ;; Accessors
   :left :right :operator :expression :value))
(in-package :lox.syntax)
(proclaim '(optimize safety))

(defclass expr ()
  ())

;; Example of defclass+ binary expansion:
;;
;; (defclass binary (expr)
;;   ((left
;;     :initarg :left
;;     :type expr)
;;    (operator
;;     :initarg :operator
;;     :type token)
;;    (right
;;     :initarg :right
;;     :type expr))
;;   (:metaclass checked-class))


(defclass+ binary (expr)
  ((left :type expr)
   (operator :type token)
   (right :type expr)))

(defclass+ grouping (expr)
  ((expression :type expr)))

(defclass+ literal (expr)
  ((value)))

(defclass+ unary (expr)
  ((operator :type token)
   (right :type expr)))

(defun make-binary (left operator right)
  (make-instance 'binary :left left :operator operator :right right))

(defun make-literal (value)
  (make-instance 'literal :value value))
