(defpackage :lox.syntax
  (:use :cl :lox.token :defclass-std :defclass+)
  (:export :expr :binary :grouping :literal :unary
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

