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
  (member x '(:NONE :FUNCTION :METHOD :INITIALIZER) :test 'eq))

(deftype current-function-type ()
  `(satisfies current-function-type-p))

(defun class-type-p (x) (member x '(:NONE :CLASS :SUBCLASS) :test 'eq))
(deftype class-type () `(satisfies class-type-p))

(defclass+ resolver ()
  ((interpreter :type lox.interpreter.def:interpreter)
   (scopes :type list
           :initform nil
           :accessor scopes)
   (current-function :type current-function-type
                     :initform :NONE
                     :accessor current-function
                     :documentation "Indicates if we're inside a function.")
   (current-class :type class-type
                  :initform :NONE
                  :accessor current-class
                  :documentation "Indicates if we're inside a class.")))

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
    `(defmethod resolve ,all-args
       ,(or documentation "")
       (with-curry (resolve resolve-local
                    declare-in-scope define-in-scope
                    begin-scope end-scope)
                   resolver
         (declare (ignorable #'resolve #'resolve-local
                             #'declare-in-scope #'define-in-scope
                             #'begin-scope #'end-scope))
         ,@body-forms))))

;;; Resolve helper methods

(defun* begin-scope ((resolver resolver))
  (push (make-hash-table :test #'equal) (scopes resolver)))

(defun* end-scope ((resolver resolver))
  (pop (scopes resolver)))

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

(defun* force-define-in-scope ((resolver resolver) (name string))
  (setf (gethash name (car (scopes resolver)))
        t))

(defun* resolve-local ((resolver resolver) (expr syntax:expr) (name lox.token:token))
  "Variable was checked to be defined in a scope.
   Resolve the local variable by determining which scope (how many jumps away)."
  (loop
    for scope-jumps from 0
    for scope in (scopes resolver)
    do
       (multiple-value-bind (_ present-p) (gethash @name.lexeme scope)
         (declare (ignore _))
         (when present-p
           (lox.interpreter::interpreter-resolve @resolver.interpreter expr scope-jumps)
           (loop-finish)))))

(defun* resolve-function ((resolver resolver) (function syntax:stmt-function)
                          (function-type current-function-type))
  (let ((enclosing-function-type (current-function resolver)))
    (setf (current-function resolver) function-type)
    (begin-scope resolver)
    (dolist (param (params function))
      (declare-in-scope resolver param)
      (define-in-scope resolver param))
    (resolve resolver (body function))
    (end-scope resolver)
    (setf (current-function resolver) enclosing-function-type)))

;;; Resolve overloads

(defgeneric resolve (resolver visited)
  (:documentation "Implements the book's visit* and resolve() functions."))

(defresolve ((statements list))
  "Resolves statements one by one."
  (dolist (statement statements)
    (resolve statement)))

(defresolve ((stmt syntax:stmt-block))
  (begin-scope)
  (resolve (statements stmt))
  (end-scope))

(defresolve ((stmt syntax:stmt-function))
  (declare-in-scope (name stmt))
  (define-in-scope  (name stmt))
  (resolve-function resolver stmt :FUNCTION))

(defresolve ((stmt syntax:stmt-var-declaration))
  (declare-in-scope (name stmt))
  (when-it (initializer stmt)
    (resolve it))
  (define-in-scope (name stmt)))

(defresolve ((expr syntax:var))
  (when-it (car (scopes resolver))
    (multiple-value-bind (value present-p) (gethash @expr.name.lexeme it)
      (when (and present-p (null value))
        (lox.error::lox-error (name expr)
                              "Cannot read local variable in its own initializer."))))
  (resolve-local expr (name expr)))

(defresolve ((expr syntax:assign))
  (resolve (value expr))
  (resolve-local expr (name expr)))

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
  (case (current-function resolver)
    (:NONE
     (lox.error:lox-error @stmt.stmt-keyword "Cannot return from top-level code."))
    (:INITIALIZER
     (lox.error:lox-error @stmt.stmt-keyword "Cannot return a value from an initializer."))
    (t
     (when-it @stmt.value
       (resolve it)))))

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

(defresolve ((expr syntax:literal))
  "No need to do anything.")

(defresolve ((expr syntax:logical))
  (resolve (left  expr))
  (resolve (right expr)))

(defresolve ((expr syntax:unary))
  (resolve (right expr)))


(defresolve ((stmt syntax:stmt-class))
  (let ((enclosing-class (current-class resolver)))
    (setf (current-class resolver) :CLASS)
    (declare-in-scope @stmt.name)
    (define-in-scope @stmt.name)

    (when @stmt.superclass
      (when (equal @stmt.name.lexeme @stmt.superclass.name.lexeme)
        (lox.error:lox-error @stmt.superclass.name
                             "A class cannot inherit from itself."))

      (begin-scope)
      (force-define-in-scope resolver "super")
      (setf (current-class resolver) :SUBCLASS)
      (resolve @stmt.superclass))

    (begin-scope)
    (setf (gethash "this" (car (scopes resolver))) t)
    (loop for method in @stmt.methods do
      (let ((declaration (if (equal "init" @method.name.lexeme)
                             :INITIALIZER
                             :METHOD)))
        (resolve-function resolver method declaration)))
    (end-scope)
    (when @stmt.superclass
      (end-scope))
    (setf (current-class resolver) enclosing-class)))


(defresolve ((expr syntax:expr-get))
  (resolve (object expr)))

(defresolve ((expr syntax:expr-set))
  (resolve @expr.value)
  (resolve @expr.object))

(defresolve ((expr syntax:this))
  (if (eq :NONE (current-class resolver))
      (lox.error:lox-error @expr.kword
                           "Cannot use 'this' outside of a class.")
      (resolve-local expr @expr.kword)))

(defresolve ((expr syntax:super))
  (case (current-class resolver)
    (:SUBCLASS
     (resolve-local expr @expr.kword))
    (:NONE 
     (lox.error:lox-error @expr.kword "Cannot use 'super' outside of a class."))
    (t ;; :CLASS
     (lox.error:lox-error @expr.kword "Cannot use 'super' in a class with no superclass."))))
