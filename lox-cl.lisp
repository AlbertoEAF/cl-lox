(defpackage :lox-cl
  (:use :defstar :defclass+ :cl-extensions))
(in-package :lox-cl)

(cl:do-symbols (s)
  (cl:import s)
  (cl:export s))

;; For some reason nil does not work the same way - I cannot import and export :cl, nil is missing:
;; (import 'cl:nil)
;; (export 'nil)


