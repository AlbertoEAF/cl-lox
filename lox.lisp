(defpackage :lox
  (:use :cl :defclass-std :lox.scanner))
(in-package :lox)
(import 'unix-opts)

(opts:define-opts
  (:name :script
   :description "script to run"
   :long "script"
   :meta-var "script"
   :arg-parser #'identity))



(defun run (source-code)
  (let* ((scanner (make-scanner source-code))
         (tokens (scan-tokens scanner)))
    (loop
      for token in tokens do (print token)
      finally (princ #\Newline))))

(defun exit (&optional (code 64))
  (sb-ext:exit :code code))

(defun run-file (filepath)
  (run (uiop:read-file-line filepath))
  (when lox.error::*had-error* (exit 64))
  )



(defun run-prompt ()
  (loop do
    (format t "> ")
    (finish-output t)
    (run (read-line))
    (setf lox.error::*had-error* nil)))

(defun main (args)
  (multiple-value-bind (options free-args) (opts:get-opts args)
    (let ((script (getf options :script)))
      (cond (free-args
             (print "Invalid usage! Pass a script or nothing at all.")
             (exit 64))
            (script
             (lox::run-file script))
            (t
             (lox::run-prompt))))))
