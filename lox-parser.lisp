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

(defmacro with-parser (parser &body body)
  `(with-curry (match check advance at-end-p peek previous consume synchronize) ,parser
     ,@body))

;;; LANGUAGE implementation

(defun* expression ((parser parser))
  (assignment parser))

(defun* statement ((parser parser))
  "Parses a lox statement."
  (with-curry (match) parser
    (cond
      ((match 'tok-type:FOR) (for-statement parser))
      ((match 'tok-type:PRINT) (print-statement parser))
      ((match 'tok-type:LEFT_BRACE) (syntax:make-stmt-block (lox-block parser)))
      ((match 'tok-type:IF) (if-statement parser))
      ((match 'tok-type:WHILE) (while-statement parser))
      (t (expression-statement parser)))))

(defun* for-statement ((parser parser))
  "Desugaring - translates to a while block."
  ;; Parsing for statement
  (with-curry (match consume check) parser
    (consume 'tok-type:LEFT_PAREN "Expect '(' after 'for'.")
    (let* ((initializer (cond ((match 'tok-type:SEMICOLON) nil)
                              ((match 'tok-type:VAR) (var-declaration parser))
                              (t (expression-statement parser))))
           (condition (prog1 (if (check 'tok-type:SEMICOLON) nil (expression parser))
                        (consume 'tok-type:SEMICOLON "Expect ';' after for loop condition.")))
           (increment (prog1 (if (check 'tok-type:RIGHT_PAREN) nil (expression parser))
                        (consume 'tok-type:RIGHT_PAREN "Expect ')'. after for clauses.")))
           (body (statement parser)))
      ;; Desugaring into an initialization block with a while body.
      ;; We start from the inner (last) forms:
      (when increment
        (setf body (syntax:make-stmt-block
                    (list body
                          (syntax:make-stmt-expression increment)))))
      (setf body (syntax:make-stmt-while (or condition (syntax:make-literal :t))
                                         body))
      (when initializer
        (setf body (syntax:make-stmt-block (list initializer body))))
      body)))



(defun* lox-declaration ((parser parser))
  "Parses the meta-statement var declaration in lox.
   It's not really a statement but a 'super' statement. It has to be parsed before regular statements."
  (handler-case
      (if (match parser 'tok-type:VAR) (var-declaration parser)
          (statement parser))
    (lox-parse-error ()
      (synchronize parser)
      nil)))

(defun* while-statement ((parser parser))
  (consume parser 'tok-type:LEFT_PAREN "Expect '(' after 'while'.")
  (let ((condition (expression parser)))
    (consume parser 'tok-type:RIGHT_PAREN "Expect ')' after condition.")
    (let ((body (statement parser)))
      (make-instance 'syntax:stmt-while :condition condition
                                        :body body))))

(defun* print-statement ((parser parser))
  (let ((expr (expression parser)))
    (consume parser 'tok-type:SEMICOLON "Expect ';' after value.")
    (make-instance 'lox.syntax:stmt-print
                   :expression expr)))

(defun* if-statement ((parser parser))
  (with-curry (consume match expression statement) parser
    (consume 'tok-type:LEFT_PAREN "Expect '(' after if.")
    (let ((condition (expression)))
      (consume 'tok-type:RIGHT_PAREN "Expect ')' after if condition.")
      (let* ((then-branch (statement))
             (else-branch (if (match 'tok-type:ELSE)
                              (statement))))
        (make-instance 'lox.syntax.stmt:stmt-if
                       :condition condition
                       :then-branch then-branch
                       :else-branch else-branch)))))



(defun* expression-statement ((parser parser))
  "An expression followed by ';'"
  (let* ((expr (expression parser)))
    (consume parser 'tok-type:SEMICOLON "Expect ';' after value.")
    (make-instance 'lox.syntax:stmt-expression
                   :expression expr)))

(defun* assignment ((parser parser))
  (let ((expr (lox-or parser)))
    (when (match parser 'tok-type:EQUAL)
      (let ((equals (previous parser)) (value (assignment parser)))
        (if (typep expr 'syntax:var)
            (let ((name (slot-value expr 'syntax:name)))
              (return-from assignment (syntax:make-assign name value)))
            (error
             (make-lox-parse-error equals "Invalid assignment target.")))))
    expr))

(defun* lox-or ((parser parser))
  (let-return (expr (lox-and parser))
    (loop while (match parser 'tok-type:OR)
          do
             (let* ((operator (previous parser))
                    (right (lox-and parser)))
               (setf expr (syntax:make-logical expr operator right))))))

(defun* lox-and ((parser parser))
  (let-return (expr (equality parser))
    (loop while (match parser 'tok-type:AND)
          do
             (let* ((operator (previous parser))
                    (right (equality parser)))
               (setf expr (syntax:make-logical expr operator right))))))

(defun* var-declaration ((parser parser))
  "Parse a var declaration statement in lox. Expects a finishing ';'."
  (let ((name (consume parser 'tok-type:IDENTIFIER "Expect variable name."))
        (initializer (if (match parser 'tok-type:EQUAL) (expression parser)
                         (syntax:make-literal :lox-unitialized-var))))
    (consume parser 'tok-type:SEMICOLON "Expect ';' after variable declaration.")
    (make-instance 'lox.syntax.stmt:stmt-var-declaration
                   :name name
                   :initializer initializer)))

(defun* lox-block ((parser parser))
  (loop while (and (not (check parser 'tok-type:RIGHT_BRACE))
                   (not (at-end-p parser)))
        collect (lox-declaration parser) into statements
        finally
           (consume parser 'tok-type:RIGHT_BRACE "Expect '}' after block.")
           (return statements)))

(defun* parse-left-associative-binary ((parser parser)
                                       parse-fn
                                       (match-operator-tokens list))
  "Parses a binary expression of the form:
    <parse-fn parser> (<operator in match-operator-tokens> <parse-fn parser>)*"
  (let-return (expr (funcall parse-fn parser))
    (loop while (match-list parser match-operator-tokens)
          do
             (setf expr (syntax:make-binary expr
                                            (previous parser)
                                            (funcall parse-fn parser))))))

(defun* equality ((parser parser))
  (let-return (expr (comparison parser))
    (loop while (match parser 'tok-type:BANG_EQUAL 'tok-type:EQUAL_EQUAL)
          do
             (setf expr (syntax:make-binary expr
                                            (previous parser)
                                            (comparison parser))))))

(defun* comparison ((parser parser))
  (let-return (expr (addition parser))
    (loop while (match parser
                  'tok-type:GREATER
                  'tok-type:GREATER_EQUAL
                  'tok-type:LESS
                  'tok-type:LESS_EQUAL)
          do
             (setf expr (syntax:make-binary expr
                                            (previous parser)
                                            (addition parser))))))

(defun* addition ((parser parser))
  (let-return (expr (multiplication parser))
    (loop while (match parser 'tok-type:MINUS 'tok-type:PLUS)
          do
             (setf expr (syntax:make-binary expr
                                            (previous parser)
                                            (multiplication parser))))))

(defun* multiplication ((parser parser))
  (let-return (expr (unary parser))
    (loop while (match parser 'tok-type:STAR 'tok-type:SLASH)
          do
             (setf expr (syntax:make-binary expr
                                            (previous parser)
                                            (unary parser))))))

(defun* unary ((parser parser))
  (if (match parser 'tok-type:BANG 'tok-type:MINUS) (syntax:make-unary (previous parser)
                                                                       (unary parser))
      (lox-call parser)))

(defun* lox-call ((parser parser))
  (let-return (expr (primary parser))
    (loop while (match parser 'tok-type:LEFT_PAREN)
          do (setf expr (finish-lox-call parser expr)))))

(defun* finish-lox-call ((parser parser) (callee syntax:expr))
  (let ((arguments))
    (when (not (check parser 'tok-type:RIGHT_PAREN))
      (loop
        for args-count below 256
        do (push (expression parser) arguments)
        while (match parser 'tok-type:COMMA)
        finally
           ;; For behaviour compatibility with the lox implementation in C:
           (if (>= args-count 255)
               ;; TODO: Shouldn't throw. Only report:
               (error (make-lox-parse-error (peek parser) "Cannot have more than 255 arguments.")))))
    (let ((paren (consume parser 'tok-type:RIGHT_PAREN "Expect ')' after arguments.")))
      (syntax:make-call callee paren (nreverse arguments)))))

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
  (loop while (not (at-end-p parser))
        collect (lox-declaration parser)))



;; (defun parse-from-source (source)
;;   "Helper method just to try things out"
;;   (lox.parser:parse
;;    (lox.parser:make-parser
;;     (lox.scanner:scan-tokens
;;      (lox.scanner:make-scanner source)))))
