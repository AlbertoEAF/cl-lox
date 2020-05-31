(defpackage :lox.parser
  (:use :cl :defclass+ :defstar :cl-extensions)
  (:export :make-parser :parse)
  (:local-nicknames (#:syntax #:lox.syntax)
                    (#:token  #:lox.token)
                    (#:tok-type #:lox.token.types)))
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
      (eq token-type
          (token:get-token-type (peek parser)))))

(defun* advance ((parser parser))
  "Consume and return token."
  (when (not (at-end-p parser))
    (incf (slot-value parser 'current)))
  (previous parser))

(defun* at-end-p ((parser parser))
  (eq 'tok-type:EOF
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
  (if (eq 'tok-type:EOF
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
                   (not (eq 'tok-type:SEMICOLON
                            (token:get-token-type (previous parser))))
                   (not (member (token:get-token-type (peek parser))
                                '(tok-type:CLASS
                                  tok-type:FUN
                                  tok-type:VAR
                                  tok-type:FOR
                                  tok-type:IF
                                  tok-type:WHILE
                                  tok-type:PRINT
                                  tok-type:RETURN))))
        do (advance parser)))

;;; LANGUAGE implementation

(defun* expression ((parser parser))
  (equality parser))

(defun* statement ((parser parser))
  (if (match parser 'tok-type:PRINT) (print-statement parser)
      (expression-statement parser)))

(defun* lox-declaration ((parser parser))
  (handler-case
      (if (match parser 'tok-type:VAR) (var-declaration parser)
          (statement parser))
    (lox-parse-error (e)
      (synchronize parser)
      nil)))

(defun* print-statement ((parser parser))
  (let ((expr (expression parser)))
    (consume parser 'tok-type:SEMICOLON "Expect ';' after value.")
    (make-instance 'lox.syntax:stmt-print
                   :expression expr)))

(defun* expression-statement ((parser parser))
  (let* ((expr (expression parser)))
    (consume parser 'tok-type:SEMICOLON "Expect ';' after value.")
    (make-instance 'lox.syntax:stmt-expression
                   :expression expr)))

(defun* var-declaration ((parser parser))
  (let ((name (consume parser 'tok-type:IDENTIFIER "Expect variable name."))
        (initializer (if (match parser 'tok-type:EQUAL) (expression parser)
                         nil)))
    (consume parser 'tok-type:SEMICOLON "Expect ';' after variable declaration.")
    (make-instance 'lox.syntax.stmt:stmt-var-declaration
                   :name name
                   :initializer initializer)))


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
    (loop while (match parser 'tok-type:BANG_EQUAL 'tok-type:EQUAL_EQUAL)
          do
             (setf expr (syntax:make-binary expr
                                            (previous parser)
                                            (comparison parser))))
    expr))

(defun* comparison ((parser parser))
  (let ((expr (addition parser)))
    (loop while (match parser
                  'tok-type:GREATER
                  'tok-type:GREATER_EQUAL
                  'tok-type:LESS
                  'tok-type:LESS_EQUAL)
          do
             (setf expr (syntax:make-binary expr
                                            (previous parser)
                                            (addition parser))))
    expr))

(defun* addition ((parser parser))
  (let ((expr (multiplication parser)))
    (loop while (match parser 'tok-type:MINUS 'tok-type:PLUS)
          do
             (setf expr (syntax:make-binary expr
                                            (previous parser)
                                            (multiplication parser))))
    expr))

(defun* multiplication ((parser parser))
  (let ((expr (unary parser)))
    (loop while (match parser 'tok-type:STAR 'tok-type:SLASH)
          do
             (setf expr (syntax:make-binary expr
                                            (previous parser)
                                            (unary parser))))
    expr))

(defun* unary ((parser parser))
  (if (match parser 'tok-type:BANG 'tok-type:MINUS) (make-instance 'syntax:unary :operator (previous parser)
                                                                                 :right (unary parser))
      (primary parser)))


(defun* primary ((parser parser))
  (with-curry (match previous) parser
    (cond
      ;; Had to disambiguate from nil (issue with false==nil in lisp):
      ((match 'tok-type:TRUE)  (syntax:make-literal :t))
      ((match 'tok-type:FALSE) (syntax:make-literal :f))
      ((match 'tok-type:NIL)   (syntax:make-literal :null))
      ((match 'tok-type:NUMBER 'tok-type:STRING)
       (syntax:make-literal (token:get-literal (previous))))
      ((match 'tok-type:IDENTIFIER)
       (syntax:make-var (previous)))
      (t (if (match 'tok-type:LEFT_PAREN) (let ((expr (expression parser)))
                                             (consume parser
                                                      'tok-type:RIGHT_PAREN
                                                      "Expect ')' after expression.")
                                             (syntax:make-grouping expr))
             (error (make-lox-parse-error (peek parser) "Expect expression.")))))))


(defun* parse ((parser parser))
  (handler-case
      (loop while (not (at-end-p parser))
            collect (lox-declaration parser))))



;; (defun parse-from-source (source)
;;   "Helper method just to try things out"
;;   (lox.parser:parse
;;    (lox.parser:make-parser
;;     (lox.scanner:scan-tokens
;;      (lox.scanner:make-scanner source)))))
