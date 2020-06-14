(defpackage :lox.interpreter
  (:use :cl :lox-cl :lox.interpreter.def :lox.interpreter.build)
  (:export :interpret :make-interpreter)
  (:local-nicknames (#:syntax #:lox.syntax)
                    (#:token  #:lox.token)
                    (#:tok-type #:lox.token.types)
                    (#:env      #:lox.environment)))
(in-package :lox.interpreter)

(named-readtables:in-readtable rutils:rutils-readtable)


(defmacro defevaluate ((&rest args) &body forms)
  "Instantiates an evaluate method which adds an interpreter as first argument and curries evaluate with it."
  (let ((all-args (append '((interpreter interpreter)) args))
        (documentation) (body-forms forms))
    (if (typep (first forms) 'string)
        (setf documentation (first body-forms)
              body-forms    (rest body-forms)))
    `(defmethod evaluate ,all-args
       ,(or documentation "")
       (with-curry (evaluate) interpreter
         (declare (ignorable #'evaluate))
         ,@body-forms))))

(defmacro defexecute ((&rest args) &body forms)
  "Instantiates an execute method which adds an interpreter as first argument and curries evaluate and execute with it. nil is automatically returned."
  (let ((all-args (append '((interpreter interpreter)) args))
        (documentation) (body-forms (append forms '(nil))))
    (if (typep (first forms) 'string)
        (setf documentation (first body-forms)
              body-forms    (rest body-forms)))
    `(defmethod execute ,all-args
       ,(or documentation "")
       (with-curry (evaluate execute) interpreter
         (declare (ignorable #'evaluate #'execute))
         ,@body-forms))))

#|
Compared to the book:
 - visit<X>Expr() -> evaluate
 - visit<X>Stmt() -> execute
|#

(defun truthy-p (expr)
  (case expr
    ((nil :false :null) :false)
    (t :true)))

(defun not-truthy-p (expr)
  (if (truthy-p expr) :false
      :true))

(defun eval-truthy-p (expr)
  (eq :true (truthy-p expr)))

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
    (error 'lox.error:lox-runtime-error
           :token operator
           :message (format nil "Operands of '~A' must be numbers." @operator.lexeme))))


(defevaluate ((expr syntax:expr))
  expr)

(defevaluate ((expr syntax:literal))
  @expr.value)

(defevaluate ((expr syntax:logical))
  (let ((left (evaluate @expr.left)))
    (cond ((eq 'tok-type:OR
               (token:get-token-type @expr.operator))
           (if (eval-truthy-p left) left))
          ((not (eval-truthy-p left)) left)
          (t (evaluate @expr.right)))))

(defevaluate ((expr syntax:grouping))
  (evaluate @expr.expression))

(defevaluate ((expr syntax:unary))
  (let ((right (evaluate @expr.right))
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
          (t (format nil "~A" obj)))))

(defevaluate ((expr syntax:binary))
  (let* ((left (evaluate  @expr.left))
         (right (evaluate @expr.right))
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

(defevaluate ((expr syntax:call))
  (let ((callee (evaluate @expr.callee))
        (arguments (mapcar #'evaluate @expr.arguments)))
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

(defevaluate ((expr syntax:var))
  "Implements visitVariableExpr but adds a check for use of uninitialized variables."
  (let* ((name @expr.name)
         (value (lookup-variable interpreter name expr)))
    (if (eq value :lox-unitialized-var)
        (error 'lox.error:lox-runtime-error
               :token name
               :message (format nil "Unitialized variable '~A'."
                                (token:get-lexeme name)))
        value)))

(defun* lookup-variable ((interpreter interpreter) (name lox.token:token) (expr syntax:expr))
  (let ((distance (gethash expr (locals interpreter))))
    (if distance
        (env:get-value-at (environment interpreter) distance name)
        (env:get-value (globals interpreter) name))))

(defevaluate ((expr syntax:assign))
  "Implements visitAssignExpr."
  (let ((name @expr.name)
        (value (evaluate @expr.value))
        (distance (gethash expr (locals interpreter))))
    (if distance
        (env:assign-at (environment interpreter) distance name value)
        (env:assign (globals interpreter) name value))))

(defevaluate ((expr syntax:expr-get))
  (let ((object (evaluate @expr.object)))
    (when (typep object 'lox.instance:lox-instance)
      (lox.instance:instance-get object @expr.name))))

(defevaluate ((expr syntax:expr-set))
  (let ((object (evaluate @expr.object)))
    (when (not (typep object 'lox.instance:lox-instance))
      (error 'lox.error:lox-runtime-error
             :token @expr.name
             :message "Only instances have fields."))
    (let ((value (evaluate @expr.value)))
      (lox.instance:instance-set object @expr.name value)
      value)))

(defevaluate ((expr syntax:this))
  (lookup-variable interpreter @expr.kword expr))

(defexecute ((stmt syntax:stmt-expression))
  (evaluate @stmt.expression))

(defexecute ((stmt syntax:stmt-function))
  "Implements visitFunctionStmt."
  (let* ((env (environment interpreter))
         (fn (lox.function:make-lox-function stmt env nil)))
    (env:define env @stmt.name.lexeme fn)))

(defexecute ((stmt syntax:stmt-print))
  (format t "~A~%" (stringify (evaluate @stmt.expression))))

(defexecute ((stmt syntax:stmt-return))
  (let ((value (if @stmt.value (evaluate @stmt.value))))
    ;; Signal with a lox-return "error" that carries the value.
    (error (make-lox-return value))))

(defexecute ((stmt syntax:stmt-var-declaration))
  (let ((value (if @stmt.initializer (evaluate @stmt.initializer))))
    (env:define @interpreter.environment (token:get-lexeme @stmt.name) value)))

(defexecute ((stmt syntax:stmt-while))
  (loop while (eval-truthy-p (evaluate @stmt.stmt-condition))
        do (execute @stmt.body)))

(defexecute ((stmt lox.syntax.stmt:stmt-block))
  (execute-block interpreter
                 @stmt.statements
                 (env:make-environment (environment interpreter))))

(defexecute ((if-stmt syntax:stmt-if))
  (cond ((eval-truthy-p (evaluate @if-stmt.stmt-condition))
         (execute @if-stmt.then-branch))
        (@if-stmt.else-branch
         (execute @if-stmt.else-branch))))

(defun* interpreter-resolve ((interpreter interpreter) (expr syntax:expr) (depth fixnum))
  (setf (gethash expr @interpreter.locals) depth))

(defun* interpret ((interpreter interpreter) (statements list))
  (handler-case
      (loop for stmt in statements
            do (execute interpreter stmt))
    (lox.error:lox-runtime-error (e)
      ;; Call runtime error handler (function with same name as condition):
      (lox.error:lox-runtime-error e))))

(defexecute ((stmt syntax:stmt-class))
  (let ((superclass))
    (when @stmt.superclass
      (setf superclass (evaluate @stmt.superclass))
      (when (not (typep superclass 'lox.class:lox-class))
        (error 'lox.error:lox-runtime-error
               :token @stmt.superclass.name
               :message "Superclass must be a class.")))
    (env:define (environment interpreter) @stmt.name.lexeme)
    (let ((methods (make-hash-table :test #'equal)))
      (dolist (method @stmt.methods)
        (setf (gethash @method.name.lexeme methods)
              (lox.function:make-lox-function method
                                              (environment interpreter)
                                              (equal "init" @method.name.lexeme))))
      (let ((class (lox.class:make-lox-class @stmt.name.lexeme superclass methods)))
        (env:assign (environment interpreter) @stmt.name class)))))

;;; Printing

(defun* count-enclosed-envs ((interpreter interpreter))
  "Counts the number of environments enclosed by the global."
  (let ((i 0))
    (loop
      with globals = (globals interpreter)
      for env = (environment interpreter) then (env::enclosing env)
      while (not (eq globals env))
      do (incf i))
    i))

(defmethod print-object ((interpreter interpreter) out)
  (let ((enclosed-envs (count-enclosed-envs interpreter)))
    (print-unreadable-object (interpreter out :identity t)
      (let ((environment (environment interpreter))
            (locals (locals interpreter))
            (globals (globals interpreter)))
        (format out "INTERPRETER #ENVS=~A #ENV-VARS=~A #GLOBALS=~A #LOCALS=~A "
                enclosed-envs
                (hash-table-count (slot-value environment 'values))
                (hash-table-count (slot-value globals 'values))
                (hash-table-count locals))
        (print-unreadable-object (environment out :identity t)
          (format out "ENV"))))))
