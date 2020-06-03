(defpackage :lox.interpreter
  (:use :cl :defstar :defclass+)
  (:export :interpret :make-interpreter)
  (:local-nicknames (#:syntax #:lox.syntax)
                    (#:token  #:lox.token)
                    (#:tok-type #:lox.token.types)
                    (#:env      #:lox.environment)))
(in-package :lox.interpreter)

#|
Compared to the book:
 - visit<X>Expr() -> evaluate call
 - visit<X>Stmt() -> execute call
|#

(defclass+ interpreter ()
  ((environment :type env:environment
                :initform (env:make-environment))))

(defun make-interpreter ()
  (make-instance 'interpreter))


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
  (slot-value expr 'syntax:value))

(defmethod evaluate ((env env:environment) (expr syntax:grouping))
  (evaluate env (slot-value expr 'syntax:expression)))

(defmethod evaluate ((env env:environment) (expr syntax:unary))
  (let ((right (evaluate env (slot-value expr 'syntax:right)))
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

(defmethod evaluate ((env env:environment) (expr syntax:binary))
  (let* ((left (evaluate env (slot-value expr 'syntax:left)))
         (right (evaluate env (slot-value expr 'syntax:right)))
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


(defmethod evaluate ((env env:environment) (expr syntax:var))
  (let* ((name (slot-value expr 'lox.syntax.expr:name))
         (value (env:get-value env name)))
    (if (eq value :lox-unitialized-var) (error 'lox.error:lox-runtime-error
                                               :token name
                                               :message (format nil "Unitialized variable '~A'."
                                                                (token:get-lexeme name)))
        value)))

(defmethod evaluate ((env env:environment) (expr syntax:assign))
  (let* ((name (slot-value expr 'syntax:name))
         (value (evaluate env (slot-value expr 'syntax:value))))
    (env:assign env name value)))

(defmethod execute ((env env:environment) (stmt syntax:stmt-expression))
  (evaluate env (slot-value stmt 'syntax:expression))
  nil)

(defmethod execute ((env env:environment) (stmt syntax:stmt-print))
  (format t "~A~%" (stringify (evaluate env (slot-value stmt 'syntax:expression))))
  nil)

(defmethod execute ((env env:environment) (stmt syntax:stmt-var-declaration))
  (with-slots (lox.syntax.stmt::name lox.syntax.stmt::initializer) stmt
    (let ((value (if lox.syntax.stmt::initializer (evaluate env lox.syntax.stmt::initializer))))
      (env:define env (token:get-lexeme lox.syntax.stmt::name) value)
      nil)))

(defun* execute-block ((statements list) (new-env env:environment))
  (loop for statement in statements do (execute new-env statement)))

(defmethod execute ((current-env env:environment) (stmt lox.syntax.stmt:stmt-block))
  (execute-block (slot-value stmt 'lox.syntax.stmt::statements)
                 (env:make-environment current-env))
  nil)

(defmethod execute ((env env:environment) (if-stmt syntax:stmt-if))
  (let ((condition (slot-value if-stmt 'LOX.SYNTAX.stmt::condition))
        (then-branch (slot-value if-stmt 'LOX.SYNTAX.stmt::then-branch))
        (else-branch (slot-value if-stmt 'LOX.SYNTAX.stmt::else-branch)))
    (cond ((eq :t (truthy-p (evaluate env condition))) (execute env then-branch))
          (else-branch (execute env else-branch))))
  nil)

(defmethod execute ((env env:environment) (stmt syntax:stmt))
  (execute stmt))

(defun* interpret ((interpreter interpreter) (statements list))
  (handler-case
      (loop for stmt in statements
            do (execute (slot-value interpreter 'environment) stmt))
    (lox.error:lox-runtime-error (e)
      ;; Call runtime error handler (function with same name as condition):
      (lox.error:lox-runtime-error e))))
