(defpackage :clox.error
  (:use :cl)
  (:export :*had-error* :lox-error :report))
(in-package :clox.error)

(defparameter *had-error* nil "Lox's error state - yup we're not using Lisp's condition system :D")

(defun clox-error (line message)
  (declare (integer line))
  (report line "" message))

(defun report (line where message)
  (declare (integer line))
  (format *error-output* "[line ~A] Error~A: ~A~%" line where message)
  (setf *had-error* T))
