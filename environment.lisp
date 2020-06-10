(defpackage :lox.environment
  (:use :cl :defclass+ :defstar :cl-extensions)
  (:export
   :environment
   :make-environment
   :define
   :assign :assign-at
   :get-value :get-value-at))
(in-package :lox.environment)

(defclass+ environment ()
  ((values :type hash-table
           :initform (make-hash-table :test 'equal))
   (enclosing :type (or null environment)
              :initform nil
              :reader enclosing)))


(defun make-environment (&optional enclosing)
  (declare (type (or null environment) enclosing))
  (make-instance 'environment :enclosing enclosing))

(defun* define ((env environment) (name string) value)
  "Used in variable declarations, to define the variable value."
  (setf (gethash name (slot-value env 'values))
        value))

(defun* assign ((env environment) (name lox.token:token) value)
  "Used to define the value of the variable in a variable declaration."
  (with-slots (values enclosing) env
    (let ((name-lexeme (lox.token:get-lexeme name)))
      (multiple-value-bind (_ present-p) (gethash name-lexeme values)
        (declare (ignore _))
        (cond (present-p (setf (gethash name-lexeme values)
                               value))
              (enclosing (assign enclosing name value))
              (t
               (lox.error:lox-runtime-error
                (make-condition 'lox.error:lox-runtime-error
                                :token name
                                :message (format nil "Undefined variable '~A'." name-lexeme)))))))))

(defun* assign-at ((env environment) (distance fixnum) (name lox.token:token) value)
  (with-slots (values) (ancestor env distance)
    (setf (gethash (lox.token:get-lexeme name) values)
          value)))

(defun* get-value ((env environment) (name lox.token:token))
  (with-slots (values enclosing) env
      (multiple-value-bind (value present-p)
          (gethash (lox.token:get-lexeme name)
                   values)
        (cond (present-p value)
              (enclosing (get-value enclosing name))
              (t
               (lox.error:lox-runtime-error
                (make-condition 'lox.error:lox-runtime-error
                                :token name
                                :message (format nil "Undefined variable '~A'."
                                                 (lox.token:get-lexeme name)))))))))

(defun* get-value-at ((env environment) (distance fixnum) (name lox.token:token))
  (with-slots (values) (ancestor env distance)
    (gethash (lox.token:get-lexeme name) values)))

(defun* ancestor ((environment environment) (distance fixnum))
  (let-return (env environment)
    (dotimes (i distance)
      (setf env (enclosing environment)))))
