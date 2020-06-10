(defpackage :lox.error
  (:use :cl)
  (:export :*had-error* :*had-runtime-error*
           :lox-error :lox-runtime-error :report
           :reset))
(in-package :lox.error)

(defparameter *had-error* nil "Lox's error state - Say wat? yup we're avoiding Lisp's condition system :D")
(defparameter *had-runtime-error* nil "Lox's runtime error state.")

(defun reset ()
  (setf *had-error* nil
        *had-runtime-error* nil))

(defmethod lox-error ((line integer) (message string))
  (report line "" message))

(defmethod lox-error ((token lox.token:token) (message string))
  (report (lox.token:get-line token) "" message))

(defun report (line where message)
  (declare (integer line))
  (format *error-output* "[line ~A] Error~A: ~A~%" line where message)
  (setf *had-error* T))

(define-condition lox-error (error)
  ((token :initarg :token
          :accessor token)
   (message :initarg :message
            :accessor message))
  (:documentation "Lox Error.")
  (:report (lambda (condition stream)
             (with-slots (token message) condition
               (format stream "Lox Error signalled: ~A~%~% TOKEN=<~A>~%"
                       message token)))))

(define-condition lox-runtime-error (lox-error)
  ()
  (:documentation "Lox Runtime Error.")
  (:report (lambda (condition stream)
             (with-slots (token message) condition
               (format stream "Lox Runtime Error signalled: ~A~%~% TOKEN=<~A>~%"
                       message token)))))


(defun lox-runtime-error (lox-runtime-error)
  (format *error-output* "[line ~A] ~A~%"
          (lox.token:get-line (token lox-runtime-error))
          (message lox-runtime-error))
  (setf *had-runtime-error* t)
  nil)
