(defpackage :lox-pprint
  (:use :cl :lox.token)
  (:local-nicknames (#:syntax #:lox.syntax)))
(in-package :lox-pprint)

(defmethod ast-pprint ((expr syntax:expr))
  (format nil "~A" expr))

(defmethod ast-pprint ((expr syntax:binary))
  (with-slots (operator left right) expr
      (parenthesize (get-lexeme operator) left right)))

(defmethod ast-pprint ((expr syntax:grouping))
  (parenthesize "group" (slot-value expr 'expression)))

(defmethod ast-pprint ((expr syntax:literal))
  (format nil "~A" (slot-value expr 'value)))

(defmethod ast-pprint ((expr syntax:unary))
  (with-slots (operator right) expr
    (parenthesize (get-lexeme operator) right)))

(defun parenthesize (name &rest exprs)
  (format nil "(~A ~{~A~^ ~})" name (mapcar #'ast-pprint exprs)))


(defparameter *demo-expr*
  (make-instance 'syntax:binary
                 :left (syntax:make-unary
                        (make-token 'MINUS "-" nil 1)
                        (syntax:make-literal 123))
                 :operator (make-token 'STAR "*" nil 1)
                 :right (syntax:make-grouping (syntax:make-literal 45.67))))

;; (ast-pprint *demo-expr*) ;; demo
