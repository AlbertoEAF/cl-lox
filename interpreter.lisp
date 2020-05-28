(defpackage :lox.interpreter
  (:use :cl :defstar :defclass+)
  (:export :interpret)
  (:local-nicknames (#:syntax #:lox.syntax)
                    (#:token  #:lox.token)
                    (#:tok-type #:lox.token.types)))
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
    (cond ((eql 'tok-type:MINUS operator-token-type)
           (- right))
          ((eql 'tok-type:BANG operator-token-type)
           (not-truthy-p right)))))

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
           "Nil")
          ((typep obj 'number) (lox-number-string-repr obj))
          (t (format nil "~S" obj)))))

(defmethod evaluate ((expr syntax:binary))
  (let* ((left (evaluate (slot-value expr 'syntax:left)))
         (right (evaluate (slot-value expr 'syntax:right)))
         (operator (slot-value expr 'syntax:operator))
         (operator-token-type (token:get-token-type operator))
         (simple-op (case operator-token-type
                      (tok-type:STAR '*)
                      (tok-type:MINUS '-)
                      (tok-type:GREATER '>)
                      (tok-type:GREATER_EQUAL '>=)
                      (tok-type:LESS '<)
                      (tok-type:LESS_EQUAL '<=))))
    (if simple-op (progn (check-number-operands operator left right)
                         (funcall simple-op left right))
        (case operator-token-type
          (tok-type:SLASH
           (if (zerop right) (error 'lox.error:lox-runtime-error
                                    :token operator
                                    :message "Division by 0.")
               (/ left right)))
          (tok-type:PLUS
           (cond ((type? 'number left right)
                  (+ left right))
                 ((type? 'string left right)
                  (format nil "~A~A" left right))
                 ((typep left 'string)
                  (format nil "~A~A" left (stringify right)))
                 ((typep right 'string)
                  (format nil "~A~A" (stringify left) right))
                 (t
                  (error 'lox.error:lox-runtime-error
                         :token operator
                         :message "Operators must be two numbers or two strings."))))
          (tok-type::BANG_EQUAL (not (is-equal left right)))
          (tok-type::EQUAL_EQUAL (is-equal left right))
          (t (error "ERROR: this is a Bug! Impossible to reach this place!"))))))


(defmethod execute ((stmt syntax:stmt-expression))
  (evaluate (slot-value stmt 'syntax:expression))
  nil)

(defmethod execute ((stmt syntax:stmt-print))
  (format t "~A~%" (stringify (evaluate (slot-value stmt 'syntax:expression))))
  nil)

(defmethod execute ((stmt syntax:stmt))
  (execute stmt))

(defun* interpret ((statements list))
  (handler-case
      (loop for stmt in statements
            do (execute stmt))
    (lox.error:lox-runtime-error (e)
      ;; Call runtime error handler (function with same name as condition):
      (lox.error:lox-runtime-error e))))
