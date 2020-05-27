(defpackage :lox.syntax
  (:use :cl :lox.token :defclass-std :defclass+)
  (:export
   ;; Syntax types
   :expr :binary :grouping :literal :unary
   ;; Constructors
   :make-binary :make-literal :make-grouping :make-unary
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

(defun make-grouping (expression)
  (make-instance 'grouping :expression expression))

(defun make-unary (operator right)
  (make-instance 'unary :operator operator :right right))

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
