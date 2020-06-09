(defpackage :lox.interpreter
  (:use :cl :defstar :defclass+ :lox.interpreter.def :lox.interpreter.build)
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


(defmethod evaluate ((interpreter interpreter) (expr syntax:expr))
  expr)

(defmethod evaluate ((interpreter interpreter) (expr syntax:literal))
  @expr.value)

(defmethod evaluate ((interpreter interpreter) (expr syntax:logical))
  (let ((left (evaluate interpreter @expr.left)))
    (cond ((eq 'tok-type:OR
               (token:get-token-type @expr.operator))
           (if (eval-truthy-p left) left))
          ((not (eval-truthy-p left)) left)
          (t (evaluate interpreter @expr.right)))))

(defmethod evaluate ((interpreter interpreter) (expr syntax:grouping))
  (evaluate interpreter @expr.expression))

(defmethod evaluate ((interpreter interpreter) (expr syntax:unary))
  (let ((right (evaluate interpreter @expr.right))
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

(defmethod evaluate ((interpreter interpreter) (expr syntax:binary))
  (let* ((left (evaluate interpreter  @expr.left))
         (right (evaluate interpreter @expr.right))
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
                         :message "+ operands must be numbers or strings."))))
          (tok-type::BANG_EQUAL (not (is-equal left right)))
          (tok-type::EQUAL_EQUAL (is-equal left right))))))

(defmethod evaluate ((interpreter interpreter) (expr syntax:call))
  (let ((callee (evaluate interpreter @expr.callee))
        (arguments (loop for argument in @expr.arguments
                         collect (evaluate interpreter argument))))
    (if (not (typep callee 'lox.callable:lox-callable))
        (error 'lox.error:lox-runtime-error
               :token @expr.paren
               :message "Can only call functions and classes."))
    (if (/= (length arguments) (lox.callable:lox-callable-arity callee))
        (error 'lox.error:lox-runtime-error
               :token @expr.paren
               :message (format nil "Expected ~A arguments but got ~A."
                                (lox.callable:lox-callable-arity callee)
                                (length arguments))))
    (lox.callable:lox-call callee interpreter arguments)))

(defmethod evaluate ((interpreter interpreter) (expr syntax:var))
  (let* ((name @expr.name)
         (value (env:get-value @interpreter.environment name)))
    (if (eq value :lox-unitialized-var) (error 'lox.error:lox-runtime-error
                                               :token name
                                               :message (format nil "Unitialized variable '~A'."
                                                                (token:get-lexeme name)))
        value)))

(defmethod evaluate ((interpreter interpreter) (expr syntax:assign))
  (let* ((name @expr.name)
         (value (evaluate interpreter @expr.value)))
    (env:assign @interpreter.environment name value)))

(defmethod execute ((interpreter interpreter) (stmt syntax:stmt-expression))
  (evaluate interpreter @stmt.expression)
  nil)

(defmethod execute ((interpreter interpreter) (stmt syntax:stmt-function))
  (let* ((current-env (environment interpreter))
         (closure-env (env:make-environment current-env))
         (fn (lox.function:make-lox-function stmt closure-env)))
    (env:define current-env @stmt.name.lexeme fn))
  nil)

(defmethod execute ((interpreter interpreter) (stmt syntax:stmt-print))
  (format t "~A~%" (stringify (evaluate interpreter @stmt.expression)))
  nil)

(defmethod execute ((interpreter interpreter) (stmt syntax:stmt-return))
  (let ((value (if @stmt.value (evaluate interpreter @stmt.value))))
    ;; Signal with a lox-return "error" that carries the value.
    (error (make-lox-return value))))


(defmethod execute ((interpreter interpreter) (stmt syntax:stmt-var-declaration))
  (let ((value (if @stmt.initializer (evaluate interpreter @stmt.initializer))))
    (env:define @interpreter.environment (token:get-lexeme @stmt.name) value)
    nil))

(defmethod execute ((interpreter interpreter) (stmt syntax:stmt-while))
  (loop while (eval-truthy-p (evaluate interpreter @stmt.condition))
        do (execute interpreter @stmt.body))
  nil)

(defmethod execute ((interpreter interpreter) (stmt lox.syntax.stmt:stmt-block))
  (execute-block @stmt.statements
                 (make-proxy-env-interpreter interpreter))
  nil)

(defmethod execute ((interpreter interpreter) (if-stmt syntax:stmt-if))
  (cond ((eval-truthy-p (evaluate interpreter @if-stmt.condition))
         (execute interpreter @if-stmt.then-branch))
        (@if-stmt.else-branch
         (execute interpreter @if-stmt.else-branch)))
  nil)



;; (defmethod execute ((interpreter interpreter) (stmt syntax:stmt))
;;    (execute stmt))

(defun* interpret ((interpreter interpreter) (statements list))
  (handler-case
      (loop for stmt in statements
            do (execute interpreter stmt))
    (lox.error:lox-runtime-error (e)
      ;; Call runtime error handler (function with same name as condition):
      (lox.error:lox-runtime-error e))))
