(defpackage :lox.resolver
  (:use :cl :lox-cl :lox.syntax :lox.token)
  (:export :resolver :make-resolver :resolve)
  (:local-nicknames (#:syntax #:lox.syntax)
                    (#:token #:lox.token)))
(in-package :lox.resolver)
(named-readtables:in-readtable rutils:rutils-readtable)

(defmacro when-it (test &body body)
  "Like WHEN. IT is bound to TEST. -- From rutils.anaphora, but import was not working."
  `(let ((it ,test))
     (when it
       ,@body)))

(defun current-function-type-p (x)
  (member x '(:NONE :FUNCTION)
          :test 'eq))

(deftype current-function-type ()
  `(satisfies current-function-type-p))

(defclass+ resolver ()
  ((interpreter :type lox.interpreter.def:interpreter)
   (scopes :type list
           :initform nil
           :accessor scopes)
   (current-function :type current-function-type
                     :initform :NONE
                     :accessor current-function)))

(defun* make-resolver ((interpreter lox.interpreter.def:interpreter))
  (make-instance 'resolver :interpreter interpreter))

(defmacro defresolve ((&rest args) &body forms)
  "Instantiates a resolve method which returns nil and adds the resolver as first method argument.
   The method resolve is also curried with the first parameter 'resolver'."
  (let ((all-args (append '((resolver resolver)) args))
        (documentation) (body-forms (append forms '(nil))))
    (if (typep (first forms) 'string)
        (setf documentation (first body-forms)
              body-forms    (rest body-forms)))
    (if documentation
        `(defmethod resolve ,all-args
           ,documentation
           (with-curry (resolve) resolver
             ,@body-forms))
        `(defmethod resolve ,all-args
           (with-curry (resolve) resolver
             ,@body-forms)))))

(defun* begin-scope ((resolver resolver))
  (push (make-hash-table :test #'equal) (scopes resolver)))

(defun* end-scope ((resolver resolver))
  (pop (scopes resolver)))

(defgeneric resolve (resolver visited)
  (:documentation "Implements the book's visit* and resolve() functions."))

(defresolve ((statements list))
  "Resolves statements one by one."
  (dolist (statement statements)
    (resolve statement)))

(defresolve ((stmt syntax:stmt-block))
  (begin-scope resolver)
  (resolve (statements stmt))
  (end-scope resolver))

(defmethod resolve ((resolver resolver) (stmt syntax:stmt-function))
  (declare-in-scope resolver (name stmt))
  (define-in-scope resolver (name stmt))
  (resolve-function resolver stmt :FUNCTION)
  nil)

(defun* resolve-function ((resolver resolver) (function syntax:stmt-function)
                          (function-type current-function-type))
  (let ((enclosing-function (current-function resolver)))
    (setf (current-function resolver) function-type)
    (begin-scope resolver)
    (dolist (param (params function))
      (declare-in-scope resolver param)
      (define-in-scope resolver param))
    (resolve resolver (body function))
    (end-scope resolver)
    (setf (current-function resolver) enclosing-function)))

(defresolve ((stmt syntax:stmt-var-declaration))
  (declare-in-scope resolver (name stmt))
  (when-it (initializer stmt)
    (resolve it))
  (define-in-scope resolver (name stmt)))

(defmethod resolve ((resolver resolver) (expr syntax:var))
  (when-it (car (scopes resolver))
    (multiple-value-bind (value present-p) (gethash @expr.name.lexeme it)
      (when (and present-p (null value))
        (error 'lox.error::lox-error :token (name expr)
                                     :message "Cannot read local variable in its own initializer."))))
  (resolve-local resolver expr (name expr))
  nil)

(defresolve ((expr syntax:assign))
  (resolve (value expr))
  (resolve-local resolver expr (name expr)))

(defun* declare-in-scope ((resolver resolver) (name lox.token:token))
  "Implements declare(). Called at variable declarations. Creates an entry in the locals hash table set to nil."
  (when-it (car (scopes resolver))
    (multiple-value-bind (_ present-p) (gethash @name.lexeme it)
      (declare (ignore _))
      (when present-p
        (lox.error:lox-error name
                   (format nil "Variable '~A' was already declared in this scope." @name.lexeme))))
    (setf (gethash @name.lexeme it)
          nil)))

(defun* define-in-scope ((resolver resolver) (name lox.token:token))
  "Called at variable definitions/assignments. Sets the hash entry to t."
  (when-it (car (scopes resolver))
    (setf (gethash @name.lexeme it)
          t)))

(defun* resolve-local ((resolver resolver) (expr syntax:expr) (name lox.token:token))
  "Variable was checked to be defined in a scope.
   Resolve the local variable by determining which scope (how many jumps away)."
  (loop
    for scope-jumps from 0
    for scope in (scopes resolver)
    when (gethash @name.lexeme scope)
      do (lox.interpreter::interpreter-resolve @resolver.interpreter expr scope-jumps)
         (loop-finish)))

(defresolve ((stmt syntax:stmt-expression))
  (resolve @stmt.expression))

(defresolve ((stmt syntax:stmt-if))
  (resolve @stmt.stmt-condition)
  (resolve @stmt.then-branch)
  (when-it @stmt.else-branch
    (resolve it)))

(defresolve ((stmt syntax:stmt-print))
  (resolve @stmt.expression))

(defresolve ((stmt syntax:stmt-return))
  (when (eq :NONE (current-function resolver))
    (lox.error:lox-error @stmt.stmt-keyword "Cannot return from top-level code."))
  (when-it @stmt.value
    (resolve it)))

(defresolve ((stmt syntax:stmt-while))
  (resolve @stmt.stmt-condition)
  (resolve @stmt.body))

(defresolve ((expr syntax:binary))
  (resolve @expr.left)
  (resolve @expr.right))

(defresolve ((expr syntax:call))
  (resolve @expr.callee)
  (dolist (argument @expr.arguments)
    (resolve argument)))

(defresolve ((expr syntax:grouping))
  "Parenthesis"
  (resolve @expr.expression))

(defmethod resolve ((resolver resolver) (expr syntax:literal))
  nil)

(defresolve ((expr syntax:logical))
  (resolve (left  expr))
  (resolve (right expr)))

(defresolve ((expr syntax:unary))
  (resolve (right expr)))