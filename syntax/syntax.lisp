(defpackage :lox.syntax
  (:use :cl :lox.token :defclass-std :checked-class)
  (:export :expr :binary :grouping :literal :unary
           :left :right :operator :expression :value))
(in-package :lox.syntax)
(proclaim '(optimize safety))

(defun with=slot-initarg (slot)
  "Add :initarg :x for slot with name x if :initarg was not set already"
  (if (getf (cdr slot) :initarg) slot
      (append slot (list :initarg (intern (string-upcase (car slot)) "KEYWORD")))))

(defmacro defclass+ (name superclasses slots)
  "Defines type-checked class. slots get a default :initarg of the same name as the slot."
  `(locally (declare (optimize safety))
     (defclass ,name ,superclasses
       ,(mapcar #'with=slot-initarg slots)
       (:metaclass checked-class))))

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

