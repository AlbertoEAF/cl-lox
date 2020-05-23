(defpackage :lox-pprint
  (:use :cl :lox.syntax :lox.token))
(in-package :lox-pprint)

(defmethod ast-pprint ((expr lox.syntax:expr))
  (format nil "~A" expr))

(defmethod ast-pprint ((expr lox.syntax:binary))
  (with-slots (operator left right) expr
      (parenthesize (lexeme operator) left right)))

(defmethod ast-pprint ((expr lox.syntax:grouping))
  (parenthesize "group" (slot-value expr 'expression)))

(defmethod ast-pprint ((expr lox.syntax:literal))
  (format nil "~A" (slot-value expr 'value)))

(defmethod ast-pprint ((expr lox.syntax:unary))
  (with-slots (operator right) expr
    (parenthesize (lexeme operator) right)))

(defun parenthesize (name &rest exprs)
  (format nil "(~A ~{~A~^ ~})" name (mapcar #'ast-pprint exprs)))


(defparameter *demo-expr*
  (make-instance 'binary
                 :left (make-instance 'unary
                                      :operator (make-token 'MINUS "-" nil 1)
                                      :right (make-instance 'literal :value 123))
                 :operator (make-token 'STAR "*" nil 1)
                 :right (make-instance 'grouping
                                       :expression (make-instance 'literal :value 45.67))))

;; (ast-pprint *demo-expr*) ;; demo
