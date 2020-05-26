(defpackage :lox.interpreter
  (:use :cl :defstar :defclass+)
  (:local-nicknames (#:syntax #:lox.syntax)
                    (#:token  #:lox.token)))
(in-package :lox.interpreter)

;; In the book the methods `evaluate` are called `visit-{literal,unary,...}`.
;; As we don't need the visitor pattern there isn't the class interpreter either.

(defun truthy-p (expr)
  (case expr
    ((nil :f :null) :f)
    (t :t)))

(defun not-truthy-p (expr)
  (if (truthy-p expr) :f
      :t))

(defun type? (type-specifier &rest vars)
  "Ensure all vars are of type type-specifier."
  (loop for var in vars always (typep var type-specifier)))

(defun lox-null (x)
  (eql x :null))

(defun is-equal (a b)
  (cond ((and (lox-null a) (lox-null b)) t)
        ((lox-null a) nil)
        (t (equal a b))))

(define-condition lox-runtime-error (error)
  ((token :initarg :token
          :accessor token)
   (message :initarg :message
            :accessor message))
  (:documentation "Lox Runtime Error."))

(defun* check-number-operand ((operator token:token) operand)
  (when (not (typep operand 'number))
    (error 'lox-runtime-error :token operator :message "Operand must be a number.")))

(defun* check-number-operands ((operator token:token) left right)
  (when (not (type? 'number left right))
    (error 'lox-runtime-error :token operator :message "Operands must be numbers.")))


(defmethod evaluate ((expr syntax:expr))
  expr)

(defmethod evaluate ((expr syntax:literal))
  (slot-value expr 'syntax:value))

(defmethod evaluate ((expr syntax:grouping))
  (evaluate (slot-value expr 'syntax:expression)))

(defmethod evaluate ((expr syntax:unary))
  (let ((right (evaluate (slot-value expr 'syntax:right)))
        (operator-token-type (token:get-token-type (slot-value expr 'syntax:operator))))
    (cond ((token:token-type= 'MINUS operator-token-type)
           (- right))
          ((token:token-type= 'BANG operator-token-type)
           (not-truthy-p right)))))

(defmethod evaluate ((expr syntax:binary))
  (let* ((left (evaluate (slot-value expr 'syntax:left)))
         (right (evaluate (slot-value expr 'syntax:right)))
         (operator (slot-value expr 'syntax:operator))
         (operator-token-type (intern
                               (string-upcase
                                (token:get-token-type operator))))
         (simple-op (case operator-token-type
                      (SLASH '/)
                      (STAR '*)
                      (MINUS '-)
                      (GREATER '>)
                      (GREATER_EQUAL '>=)
                      (LESS '<)
                      (LESS_EQUAL '<=))))
    (if simple-op (progn (check-number-operands operator left right)
                         (funcall simple-op left right))
        (case operator-token-type
          (PLUS
           (cond ((type? 'string left right)
                  (format nil "~A~A" left right))
                 ((type? 'number left right)
                  (+ left right))
                 (t
                  (error 'lox-runtime-error :token operator
                                            :message "Operators must be two numbers or two strings."))))
          (BANG_EQUAL (not (is-equal left right)))
          (EQUAL_EQUAL (is-equal left right))
          (t nil)))))
