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

(defun* check-number-operand ((operator token:token) operand)
  (when (not (typep operand 'number))
    (error 'lox.error:lox-runtime-error :token operator :message "Operand must be a number.")))

(defun* check-number-operands ((operator token:token) left right)
  (when (not (type? 'number left right))
    (error 'lox.error:lox-runtime-error :token operator :message "Operands must be numbers.")))


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
                  (error 'lox.error:lox-runtime-error :token operator
                                            :message "Operators must be two numbers or two strings."))))
          (BANG_EQUAL (not (is-equal left right)))
          (EQUAL_EQUAL (is-equal left right))
          (t nil)))))

(defun stringify (obj)
  (flet ((lox-number-string-repr (num)
           (let* ((num-str (format nil "~A" num)))
             (cond ((str:ends-with? ".0" num-str)
                    (subseq num-str 0
                            (- (length num-str) 2)))
                   ((str:ends-with? ".0d0" num-str)
                    (subseq num-str 0
                            (- (length num-str) 4)))
                   (t num-str)))))
    (cond ((or (null obj)
               (eql obj :null))
           "nil")
          ((typep obj 'number) (lox-number-string-repr obj))
          (t (format nil "~A" obj)))))


(defun* interpret ((expression syntax:expr))
  (handler-case
      (format t "~A" (stringify (evaluate expression)))
    (lox.error:lox-runtime-error (e)     ;; condition
      (lox.error:lox-runtime-error e)))) ;; function
