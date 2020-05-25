(defpackage :lox.token
  (:use :cl :cl-interpol :defclass+)
  (:export
   ;; class and high-level methods
   :token :token-type :make-token
   ;; token slot readers
   :get-lexeme :get-token-type :get-literal :get-line
   ;; utils
   :to-string :lexeme :token-type=))

(in-package :lox.token)

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

(defun token-type= (a b)
  (string= a b))

(defun token-type-p (x) (member x *token-type* :test #'string=))
(deftype token-type () `(satisfies token-type-p))

(defclass+ token ()
  ((token-type
    :reader get-token-type
    :type token-type)
   (lexeme
    :reader get-lexeme
    :type string)
   (literal
    :reader get-literal)
   (line
    :reader get-line
    :type integer)))

(defun make-token (token-type lexeme literal line)
  (declare (integer line))
  (make-instance 'token :token-type token-type
                        :lexeme lexeme
                        :literal literal
                        :line line))

(defmethod to-string ((token token))
  (with-slots (token-type lexeme literal) token
    (format nil #?"${token-type} ${lexeme} ${literal}")))

(defmethod print-object ((token token) out)
  (print-unreadable-object (token out :type t :identity t)
    (with-slots (token-type lexeme literal line) token
      (format out "~A lex=~A lit=~A @line=~A"
              token-type lexeme literal line))))
