(defpackage :clox/token
  (:use :cl :defclass-std :cl-interpol :checked-class))
(in-package :clox/token)

(named-readtables:in-readtable :interpol-syntax)

(defparameter *token-type*
  '(
    ;; Single-character tokens.
    LEFT_PAREN RIGHT_PAREN LEFT_BRACE RIGHT_BRACE
    COMMA DOT MINUS PLUS SEMICOLON SLASH STAR

    ;; One or two character tokens.
    BANG BANG_EQUAL
    EQUAL EQUAL_EQUAL
    GREATER GREATER_EQUAL
    LESS LESS_EQUAL

    ;; Literals.
    IDENTIFIER STRING NUMBER

    ;; Keywords.
    AND CLASS ELSE FALSE FUN FOR IF NIL OR
    PRINT RETURN SUPER THIS TRUE VAR WHILE

    EOF))

(defun token-type-p (x) (member x *token-type*))
(deftype token-type () `(satisfies token-type-p))

(locally (declare (optimize safety))
  (defclass token ()
    ((token-type
      :initarg :token-type
      :reader token-type
      :type token-type)
     (lexeme
      :initarg :lexeme
      :reader lexeme
      :type string)
     (literal
      :initarg :literal
      :reader literal)
     (line
      :initarg :line
      :reader line
      :type integer))
    (:metaclass checked-class::checked-class)))

(defun make-token (token-type &key lexeme literal line)
  (declare (integer line))
  (make-instance 'token :token-type token-type
                        :lexeme lexeme
                        :literal literal
                        :line line))

(defmethod to-string ((token token))
  (with-slots (token-type lexeme literal) token
    (format nil #?"${token-type} ${lexeme} ${literal}")))
