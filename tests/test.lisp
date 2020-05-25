(defpackage :lox/tests
  (:use :cl :lox))
(in-package :lox/tests)

(string= "2"
         (run " 2 "))

(run "2+3 - 5 +3")

(run "2+3 - 5 / 3")

(run "5/3")
