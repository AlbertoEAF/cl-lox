(defpackage :lox.interpreter
  (:use :cl :defstar :defclass+)
  (:export :interpret :make-interpreter)
  (:local-nicknames (#:syntax #:lox.syntax)
                    (#:token  #:lox.token)
                    (#:tok-type #:lox.token.types)
                    (#:env      #:lox.environment)))
(in-package :lox.interpreter)

(named-readtables:in-readtable rutils:rutils-readtable)

#|
Compared to the book:
 - visit<X>Expr() -> evaluate call
 - visit<X>Stmt() -> execute call
|#

(defclass+ interpreter ()
  ((globals :type env:environment)
   (environment :type env:environment)))

(defun make-interpreter ()
  (let* ((globals (env:make-environment))
         (environment (env:make-environment globals)))
    (flet ((define-global-function (name fn &optional (repr "<native fn>"))
             "Helper to define global a function."
             (env:define globals name
               (lox.callable:make-lox-function
                name (length (trivial-arguments:arglist fn)) fn repr))))
      (define-global-function "clock" (lambda () (get-universal-time)))
      (define-global-function "readfile" (lambda (fpath) (uiop:read-file-string fpath)))
      (define-global-function "readline" (lambda () (read-line))))
    (make-instance 'interpreter :globals globals :environment environment)))


(defgeneric evaluate (env expr)
  (:documentation "For expressions. Equivalent to lox's visit<X>Expr."))

(defgeneric execute (env stmt)
  (:documentation "For statements. Equivalent to lox's visit<X>Stmt"))

(defun truthy-p (expr)
  (case expr
    ((nil :f :null) :f)
    (t :t)))

(defun not-truthy-p (expr)
  (if (truthy-p expr) :f
      :t))

(defun eval-truthy-p (expr)
  (eq :t (truthy-p expr)))

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


(defmethod evaluate ((env env:environment) (expr syntax:expr))
  expr)

(defmethod evaluate ((env env:environment) (expr syntax:literal))
  @expr.value)

(defmethod evaluate ((env env:environment) (expr syntax:logical))
  (let ((left (evaluate env @expr.left)))
    (cond ((eq 'tok-type:OR
               (token:get-token-type @expr.operator))
           (if (eval-truthy-p left) left))
          ((not (eval-truthy-p left)) left)
          (t (evaluate env @expr.right)))))

(defmethod evaluate ((env env:environment) (expr syntax:grouping))
  (evaluate env @expr.expression))

(defmethod evaluate ((env env:environment) (expr syntax:unary))
  (let ((right (evaluate env @expr.right))
        (operator-token-type (token:get-token-type @expr.operator)))
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

(defmethod evaluate ((env env:environment) (expr syntax:binary))
  (let* ((left (evaluate env  @expr.left))
         (right (evaluate env @expr.right))
         (operator @expr.operator)
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
        (ecase operator-token-type
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
          (tok-type::EQUAL_EQUAL (is-equal left right))))))

(defmethod evaluate ((env env:environment) (expr syntax:call))
  (let ((callee (evaluate env @expr.callee))
        (arguments (loop for argument in @expr.arguments
                         collect (evaluate env argument))))
    (if (not (typep callee 'lox.callable:lox-callable))
        (error 'lox.error:lox-runtime-error
               :token @expr.paren
               :message "Can only call functions and classes."))
    (let ((lox-function callee))
      (if (/= (length arguments) (lox.callable:lox-callable-arity lox-function))
          (error 'lox.error:lox-runtime-error
                 :token @expr.paren
                 :message (format nil "Expected ~A arguments but got ~A."
                                  (lox.callable:lox-callable-arity lox-function)
                                  (length arguments))))
      (lox.callable:lox-call lox-function env arguments))))

;; (defmethod find-lox-function ((env env:environment) callee)
;;   (let ((lox-function (env:get-value env callee)))))

(defmethod evaluate ((env env:environment) (expr syntax:var))
  (let* ((name @expr.name)
         (value (env:get-value env name)))
    (if (eq value :lox-unitialized-var) (error 'lox.error:lox-runtime-error
                                               :token name
                                               :message (format nil "Unitialized variable '~A'."
                                                                (token:get-lexeme name)))
        value)))

(defmethod evaluate ((env env:environment) (expr syntax:assign))
  (let* ((name @expr.name)
         (value (evaluate env @expr.value)))
    (env:assign env name value)))

(defmethod execute ((env env:environment) (stmt syntax:stmt-expression))
  (evaluate env @stmt.expression)
  nil)

(defmethod execute ((env env:environment) (stmt syntax:stmt-print))
  (format t "~A~%" (stringify (evaluate env @stmt.expression)))
  nil)

(defmethod execute ((env env:environment) (stmt syntax:stmt-var-declaration))
  (with-slots (lox.syntax.stmt::name lox.syntax.stmt::initializer) stmt
    (let ((value (if @stmt.initializer (evaluate env @stmt.initializer))))
      (env:define env (token:get-lexeme @stmt.name) value)
      nil)))

(defmethod execute ((env env:environment) (stmt syntax:stmt-while))
  (loop while (eval-truthy-p (evaluate env @stmt.condition))
        do (execute env @stmt.body))
  nil)

(defun* execute-block ((statements list) (new-env env:environment))
  (loop for statement in statements do (execute new-env statement)))

(defmethod execute ((current-env env:environment) (stmt lox.syntax.stmt:stmt-block))
  (execute-block @stmt.statements
                 (env:make-environment current-env))
  nil)

(defmethod execute ((env env:environment) (if-stmt syntax:stmt-if))
  (cond ((eq :t (truthy-p (evaluate env @if-stmt.condition))) (execute env @if-stmt.then-branch))
        (@if-stmt.else-branch (execute env @if-stmt.else-branch)))
  nil)

(defmethod execute ((env env:environment) (stmt syntax:stmt))
  (execute stmt))

(defun* interpret ((interpreter interpreter) (statements list))
  (handler-case
      (loop for stmt in statements
            do (execute @interpreter.environment stmt))
    (lox.error:lox-runtime-error (e)
      ;; Call runtime error handler (function with same name as condition):
      (lox.error:lox-runtime-error e))))
