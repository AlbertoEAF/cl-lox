(defpackage :clox
  (:use :cl :defclass-std :clox/parser))
(in-package :clox)
(import 'unix-opts)

(opts:define-opts
  (:name :script
   :description "script to run"
   :long "script"
   :meta-var "script"
   :arg-parser #'identity))

(defparameter *had-error* nil "Lox's error state - yup we're not using Lisp's condition system :D")

(defclass/std scanner ()
  ((content)))

(defun delimiterp (c) (or (char= c #\Space) (char= c #\Newline)))

(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
          :then (position-if-not delimiterp string :start (1+ end))
        :for end = (and beg (position-if delimiterp string :start beg))
        :when beg :collect (subseq string beg end)
          :while end))

(defmethod scan-tokens ((scanner scanner))
  (with-slots (content) scanner
    (my-split content)))

(defun run (source-code)
  (let* ((scanner (make-instance 'scanner :content source-code))
         (tokens (scan-tokens scanner)))
    (loop
      for token in tokens do (print token)
      finally (princ #\Newline))))

(defun exit (&optional (code 64))
  (sb-ext:exit :code code))

(defun run-file (filepath)
  (run (uiop:read-file-line filepath))
  (when *had-error* (exit 64))
  )

(defun lox-error (line message)
  (declare (integer line))
  (report line "" message))

(defun report (line where message)
  (declare (integer line))
  (format *error-output* "[line ~A] Error~A: ~A" line where message))

(defun run-prompt ()
  (loop do
    (format t "> ")
    (finish-output t)
    (run (read-line))
    (setf *had-error* nil)))

(defun main (args)
  (multiple-value-bind (options free-args) (opts:get-opts args)
    (let ((script (getf options :script)))
      (cond (free-args
             (print "Invalid usage! Pass a script or nothing at all.")
             (exit 64))
            (script
             (clox::run-file script))
            (t
             (clox::run-prompt))))))
