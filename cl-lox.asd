(defsystem "cl-lox"
  :depends-on ("cl-interpol" "cl-graph" "trivial-arguments"
                             "queues" "log4cl" "array-operations"
                             "alexandria" "defclass-std" "iterate"
                             "unix-opts" "defenum" "closer-mop"
                             "defstar")
  :serial t
  :components (;; Utils (can be moved to external libraries)
               (:file "checked-class")
               (:file "defclass-plus")
               ;; Lox code
               (:file "lox-error")
               (:file "lox-token")
               (:file "scanner")
               (:file "syntax/syntax")
               (:file "syntax/pprint")
               (:file "lox-parser")
               (:file "lox")))

;; (defsystem "aoc19/tests"
;;   :depends-on ("aoc19" "rove")
;;   :components ((:file "day05-tests")
;;                (:file "intcode-tests")))
