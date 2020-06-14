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

(defun* define ((env environment) (name string) &optional (value nil))
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
  "Implements get"
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

(defmethod get-value-at ((env environment) (distance fixnum) (name lox.token:token))
  "Implements getAt."
  (let ((ancestor (ancestor env distance)))
    (with-slots (values) ancestor
      (multiple-value-bind (value present-p) (gethash (lox.token:get-lexeme name) values)
        (assert present-p ()
                "get-value-at failed to fetch variable '~A' from ~A's ancestor ~A at distance ~A."
                name env (ancestor env distance) distance)
        value))))

(defmethod get-value-at ((env environment) (distance fixnum) (name string))
  "Implements getAt."
  (let ((ancestor (ancestor env distance)))
    (with-slots (values) ancestor
      (multiple-value-bind (value present-p) (gethash name values)
        (assert present-p ()
                "get-value-at failed to fetch variable '~A' from ~A's ancestor ~A at distance ~A."
                name env (ancestor env distance) distance)
        value))))


(defun* ancestor ((environment environment) (distance fixnum))
  (let-return (env environment)
    (dotimes (i distance)
      (setf env (enclosing env)))))

(defmethod print-object ((env environment) out)
  (print-unreadable-object (env out :identity t)
    (let ((enclosing (enclosing env)))
      (format out "ENV #VALUES=~A ENCLOSING=" (hash-table-count (slot-value env 'values)))
      (print-unreadable-object (enclosing out :identity t)
        (format out "ENV")))))
