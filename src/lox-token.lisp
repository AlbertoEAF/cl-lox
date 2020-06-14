(defpackage :lox.token
  (:use :cl :cl-interpol :defclass+)
  (:export
   ;; class and high-level methods
   :token :token-type :make-token
   ;; token slot readers
   :get-lexeme :get-token-type :get-literal :get-line
   ;; utils
   :to-string :lexeme))

(in-package :lox.token)

(named-readtables:in-readtable :interpol-syntax)

(defun token-type-p (x) (member x lox.token.types::*token-types* :test 'eq))
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
  (print-unreadable-object (token out)
    (with-slots (token-type lexeme literal line) token
      (format out "Token:~A '~A'"
              token-type lexeme literal line))))
