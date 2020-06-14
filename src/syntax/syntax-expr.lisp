(defpackage :lox.syntax.expr
  (:use :cl :lox.token :defclass-std :defclass+)
  (:export
   ;; Base type
   :expr
   ;; Expr and constructors
   :binary   :make-binary
   :grouping :make-grouping
   :literal  :make-literal
   :unary    :make-unary
   :var      :make-var
   :assign   :make-assign
   :logical  :make-logical
   :call     :make-call
   :expr-get :make-expr-get
   :expr-set :make-expr-set
   :this     :make-this
   :super    :make-super
   ;; Accessors
   :left :right :operator :expression :value :assign :name
   :callee :paren :arguments :object :kword :lox-method))
(in-package :lox.syntax.expr)
(proclaim '(optimize safety))

(defclass expr ()
  ())

(defmacro defsyntax (name superclasses slots)
  "Creates a class with default initarg and accessors and constructor."
  `(defclass++ ,name ,superclasses ,slots
     :slot-enhancers (defclass+::with=default-initarg
                      defclass+::with=default-accessor)))

(defsyntax assign (expr)
  ((name :type token)
   (value :type expr)))

(defsyntax binary (expr)
  ((left :type expr)
   (operator :type token)
   (right :type expr)))

(defsyntax grouping (expr)
  ((expression :type expr)))

(defsyntax literal (expr)
  ((value)))

(defsyntax unary (expr)
  ((operator :type token)
   (right :type expr)))

(defsyntax var (expr)
  ((name :type token)))

(defsyntax logical (expr)
  ((left :type expr)
   (operator :type token)
   (right :type expr)))

(defsyntax call (expr)
  ((callee :type expr)
   (paren :type token)
   (arguments :type list)))

(defsyntax expr-get (expr)
  ((object :type expr)
   (name :type token)))

(defsyntax expr-set (expr)
  ((object :type expr)
   (name :type token)
   (value :type expr)))

(defsyntax this (expr)
  ((kword :type token)))

(defsyntax super (expr)
  ((kword :type token)
   (lox-method :type token)))

;;; Printing

(defun get-slot-names (clos-obj)
  "Fetch all slot names from object (so prints are dynamic for any expr class)."
  (mapcar #'sb-mop:slot-definition-name
          (sb-mop:class-direct-slots
           (class-of clos-obj))))

(defmethod print-object ((expr expr) out)
  (print-unreadable-object (expr out)
    (format out "EXPR")
    (loop
      for slot-name in (get-slot-names expr)
      for slot-value = (slot-value expr slot-name)
      do (format out " ~A=~A" slot-name slot-value))))
