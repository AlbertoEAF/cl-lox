(defpackage :lox.syntax.stmt
  (:use :cl :defclass-std :defclass+)
  (:import-from :lox.syntax.expr :expr :expression :defsyntax :value :name)
  (:import-from :lox.token :token)
  (:export
   ;; Statement classes
   :stmt
   :stmt-expression
   :stmt-print
   :stmt-var-declaration
   :stmt-block
   :stmt-if
   :stmt-while
   :stmt-function
   :stmt-return
   ;; Constructors
   :make-stmt-expression
   :make-stmt-print
   :make-stmt-var-declaration
   :make-stmt-block
   :make-stmt-if
   :make-stmt-while
   :make-stmt-function
   :make-stmt-return
   ;; Classes and constructors
   :stmt-class :make-stmt-class
   ;; Accessors
   :expression :name :initializer :statements :stmt-condition :then-branch :else-branch :body :params :stmt-keyword :value :methods))
(in-package :lox.syntax.stmt)

(defclass+ stmt ()
  ())

(defsyntax stmt-expression (stmt)
  ((expression :type expr)))

(defsyntax stmt-print (stmt)
  ((expression :type expr)))

(defsyntax stmt-var-declaration (stmt)
  ((name :type token)
   (initializer :type (or null expr))))

(defsyntax stmt-block (stmt)
  ;; Probably could add checking of individual items at introduction (list stmt*)
  ((statements :type list)))

(defsyntax stmt-if (stmt)
  ((stmt-condition :type expr)
   (then-branch :type stmt)
   (else-branch :type (or null stmt))))

(defsyntax stmt-while (stmt)
  ((stmt-condition :type expr)
   (body :type stmt)))

(defsyntax stmt-function (stmt)
  ((name :type token)
   (params :type list)
   (body :type list)))

(defsyntax stmt-return (stmt)
  ((stmt-keyword :type token)
   (value :type expr)))

(defsyntax stmt-class (stmt)
  ((name :type token)
   (methods :type list)))

(defmethod print-object ((stmt stmt) out)
  (print-unreadable-object (stmt out :identity t)
    (format out "~A" (class-name (class-of stmt)))
    (print-details stmt out)))

(defgeneric print-details (stmt out)
  (:documentation "Helper function to print-object for children of stmt. Prints the stmt details."))

(defmethod print-details ((stmt stmt) out)
  "By default no details are printed.")

(defmethod print-details ((stmt stmt-function) out)
  (format out " ~A" (lox.token:get-lexeme (name stmt))))

(defmethod print-details ((stmt stmt-var-declaration) out)
  (format out " ~A" (lox.token:get-lexeme (name stmt))))

