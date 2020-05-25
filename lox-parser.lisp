(defpackage :lox.parser
  (:use :cl :defclass+ :defstar)
  (:export :make-parser :parse)
  (:local-nicknames (#:syntax #:lox.syntax)
                    (#:token  #:lox.token)))
(in-package :lox.parser)

;;; Parser base code

(defclass+ parser ()
  ((tokens  :type list    :initform nil)
   (current :type integer :initform 0)))

(defun make-parser (tokens)
  (make-instance 'parser :tokens tokens))

(defun* match-list ((parser parser) token-types)
  "Advance and return T if it matches one of the token-types."
  (when (loop for x in token-types
                thereis (check parser x))
    (advance parser)
    t))

(defun* match ((parser parser) &rest token-types)
  "Advance and return T if it matches one of the token-types."
  (when (loop for x in token-types
                thereis (check parser x))
    (advance parser)
    t))

(defun* check ((parser parser) (token-type token:token-type))
  "Returns Boolean. Checks token type."
  (if (at-end-p parser) nil
      (token:token-type= token-type
                         (token:get-token-type (peek parser)))))

(defun* advance ((parser parser))
  "Consume and return token."
  (when (not (at-end-p parser))
    (incf (slot-value parser 'current)))
  (previous parser))

(defun* at-end-p ((parser parser))
  (token:token-type= 'EOF
                     (token:get-token-type (peek parser))))

(defun* peek ((parser parser))
  "Peek token."
  (with-slots (tokens current) parser
    (elt tokens current)))

(defun* previous ((parser parser))
  "Returns the previous token."
  (with-slots (tokens current) parser
    (elt tokens (1- current))))

(define-condition lox-parse-error (error)
  ((token :initarg :token
          :initform nil
          :reader token)
   (message :initarg :message
            :initform "Generic Lox parser error."
            :reader :message))
  (:documentation "Error from the lox parser."))

(defun* make-lox-parse-error ((token token:token) (message string))
  "Create condition, report in lox and return condition-"
  ;; Could be so much simpler if we weren't following the book so literally.
  (if (token:token-type= 'EOF
                         (token:get-token-type token))
      (lox.error:report (token:get-line token)
                        " at end" message)
      (lox.error:report (token:get-line token)
                        (format nil " at '~A'" (token:get-lexeme token))
                        message))
  (make-condition 'lox-parse-error :token token :message message))


(defun* consume ((parser parser) (token-type token:token-type) (message string))
  (if (check parser token-type) (advance parser)
      (error (make-lox-parse-error (peek parser) message))))

(defun* synchronize ((parser parser))
  (advance parser)
  (loop while (and (not (at-end-p parser))
                   (not (token:token-type= 'SEMICOLON
                                           (token:get-token-type (previous parser))))
                   (not (member (token:get-token-type (peek parser))
                                '(CLASS FUN VAR FOR IF WHILE PRINT RETURN)
                                :test #'token:token-type=)))
        do (advance parser)))

;;; LANGUAGE implementation

(defun* expression ((parser parser))
  (equality parser))

(defun* parse-left-associative-binary ((parser parser)
                                       parse-fn
                                       (match-operator-tokens list))
  "Parses a binary expression of the form:
    <parse-fn parser> (<operator in match-operator-tokens> <parse-fn parser>)*"
  (let ((expr (funcall parse-fn parser)))
    (loop while (match-list parser match-operator-tokens)
          do
             (setf expr (syntax:make-binary expr
                                            (previous parser)
                                            (funcall parse-fn parser))))
    expr))

(defun* equality ((parser parser))
  (let ((expr (comparison parser)))
    (loop while (match parser 'BANG_EQUAL 'EQUAL_EQUAL)
          do
             (setf expr (syntax:make-binary expr
                                            (previous parser)
                                            (comparison parser))))
    expr))

(defun* comparison ((parser parser))
  (let ((expr (addition parser)))
    (loop while (match parser 'GREATER 'GREATER_EQUAL 'LESS 'LESS_EQUAL)
          do
             (setf expr (syntax:make-binary expr
                                            (previous parser)
                                            (addition parser))))
    expr))

(defun* addition ((parser parser))
  (let ((expr (multiplication parser)))
    (loop while (match parser 'MINUS 'PLUS)
          do
             (setf expr (syntax:make-binary expr
                                            (previous parser)
                                            (multiplication parser))))
    expr))

(defun* multiplication ((parser parser))
  (let ((expr (unary parser)))
    (loop while (match parser 'STAR 'SLASH)
          do
             (setf expr (syntax:make-binary expr
                                            (previous parser)
                                            (unary parser))))
    expr))

(defun* unary ((parser parser))
  (if (match parser 'BANG 'MINUS) (make-instance 'syntax:unary :operator (previous parser)
                                                                :right (unary parser))
      (primary parser)))


(defun* primary ((parser parser))
  (cond
    ;; Had to disambiguate from nil (issue with false==nil in lisp):
    ((match parser 'TRUE)  (syntax:make-literal :t))
    ((match parser 'FALSE) (syntax:make-literal :f))
    ((match parser 'NIL)   (syntax:make-literal :null))
    ((match parser 'NUMBER 'STRING)
     (syntax:make-literal (token:get-literal (previous parser))))
    (t (if (match parser 'LEFT_PAREN) (let ((expr (expression parser)))
                                        (consume parser 'RIGHT_PAREN "Expect ')' after expression.")
                                        (make-instance 'syntax:grouping
                                                       :expression expr))
           (error (make-lox-parse-error (peek parser) "Expect expression."))))))


(defun* parse ((parser parser))
  (handler-case
      (expression parser)
    (lox-parse-error (e)
      (with-slots (token message) e
        (format t "ERROR ~A ~% tok=~A~% msg=~A)~%" e token message)))))
