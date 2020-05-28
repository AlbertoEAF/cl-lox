(defpackage :lox
  (:use :cl :defclass-std :lox.scanner :lox.parser :lox.pprint)
  (:export :run :run-prompt :main))
(in-package :lox)
(import 'unix-opts)

(opts:define-opts
  (:name :script
   :description "script to run"
   :long "script"
   :meta-var "script"
   :arg-parser #'identity))

(defun run (source-code)
  (declare (type string source-code))
  (lox.error:reset)
  (let* ((scanner (make-scanner source-code))
         (tokens (scan-tokens scanner))
         (parser (make-parser tokens))
         (statements (parse parser)))
    (when (null lox.error::*had-error*)
      (princ (lox.interpreter:interpret statements)))))

(defun exit (&optional (code 65))
  (sb-ext:exit :code code))

(defun run-file (filepath)
  (run (uiop:read-file-line filepath))
  (when lox.error::*had-error* (exit 65))
  (when lox.error::*had-runtime-error* (exit 70)))

(defun run-prompt ()
  (loop do
    (format t "> ")
    (finish-output t)
    (run (read-line))
    (format t "~%")
    (setf lox.error::*had-error*         nil
          lox.error::*had-runtime-error* nil)))

(defun main (args)
  (multiple-value-bind (options free-args) (opts:get-opts args)
    (let ((script (getf options :script)))
      (cond (free-args
             (print "Invalid usage! Pass a script or nothing at all.")
             (exit 65))
            (script
             (lox::run-file script))
            (t
             (lox::run-prompt))))))
