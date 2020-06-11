(defpackage :lox
  (:use :cl :defclass-std :lox.scanner :lox.parser :lox.pprint)
  (:export :run :run-prompt :run-file :main))
(in-package :lox)
(import 'unix-opts)

(opts:define-opts
  (:name :script
   :description "script to run"
   :long "script"
   :meta-var "script"
   :arg-parser #'identity))

(defun exit (&optional (code 65))
  (sb-ext:exit :code code))

(defun read-multi-line ()
  "Reads a list of lines (strings) until a line is empty."
  (loop for line = (read-line)
        until (equal "" line)
        collect line))

(defun concatenate-lines (lines)
  (format nil "~{~A~^~%~}" lines))

(defun read-multi-line-string ()
  "Stops reading on an empty line."
  (concatenate-lines (read-multi-line)))

(defun run (source-code)
  (declare (type string source-code))
  (lox.error:reset)
  (let* ((scanner (make-scanner source-code))
         (tokens (scan-tokens scanner))
         (parser (make-parser tokens))
         (statements (parse parser))
         (interpreter (lox.interpreter:make-interpreter)))
    (when (null lox.error::*had-error*)
      (let ((resolver (lox.resolver:make-resolver interpreter)))
        (lox.resolver:resolve resolver statements)
        (when (null lox.error::*had-error*)
          (princ (lox.interpreter:interpret interpreter statements)))))))

(defun run-file (filepath &key (exit t))
  (format t "Running file ~A.~%~%" filepath)
  (run (concatenate-lines (uiop:read-file-lines filepath)))
  (when exit
    (when lox.error::*had-error* (exit 65))
    (when lox.error::*had-runtime-error* (exit 70))))

(defun print-header (msg &key
                           (header-char #\-) (header-width 79) (spacing 1)
                           (pre-lines 3) (post-lines 1))
  (flet ((nprint (char n)
           (dotimes (_ n)
             (write-char char))))
    (let* ((msg-len (length msg))
           (header-chars-count (- header-width (* 2 spacing) msg-len)))
      (multiple-value-bind (per-side-chars remainder-chars) (truncate header-chars-count 2)
        (nprint #\Newline pre-lines)
        (nprint header-char per-side-chars)
        (nprint #\Space spacing)
        (princ msg)
        (nprint #\Space spacing)
        (nprint header-char (+ per-side-chars remainder-chars))
        (nprint #\Newline post-lines)))))

(defun run-demos (folder &key (stop-on-error nil) (exit nil))
  (let ((files (remove-if-not (lambda (p) (str:ends-with-p ".lox" (namestring p)))
                              (uiop:directory-files folder))))
    (format nil "~%### Running ~A demos ###" (length files))
    (dolist (file files)
      (print-header (str:concat "Demo " (pathname-name file)))
      (lox:run-file file :exit exit)
      (when (and stop-on-error
                 (or lox.error::*had-error*
                     lox.error::*had-runtime-error*))
        (return)))))

(defun run-prompt ()
  (loop do
    (format t "> ")
    (finish-output t)
    (run (read-multi-line-string))
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
