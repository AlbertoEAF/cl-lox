(defsystem "cl-lox"
  :depends-on ("cl-interpol" "cl-graph" "trivial-arguments"
                             "queues" "log4cl" "array-operations"
                             "alexandria" "defclass-std" "iterate"
                             "unix-opts" "defenum" "closer-mop"
                             "defstar" "str" "rutils")
  ;;:serial t
  :components (;; Utils (can be moved to external libraries)
               (:file "helpers/checked-class")
               (:file "helpers/defclass-plus" :depends-on ("helpers/checked-class"))
               (:file "helpers/cl-extensions")
               ;; Lox code
               (:file "lox-token-types")
               (:file "lox-token" :depends-on ("lox-token-types"))
               (:file "lox-error" :depends-on ("lox-token"))
               (:file "scanner" :depends-on ("lox-token" "lox-error"))
               (:file "syntax/syntax-expr" :depends-on ("lox-token"))
               (:file "syntax/syntax-stmt" :depends-on ("lox-token"))
               (:file "syntax/syntax" :depends-on ("syntax/syntax-expr" "syntax/syntax-stmt"))
               (:file "syntax/pprint" :depends-on ("syntax/syntax"))
               (:file "environment" :depends-on ("lox-error"))
               (:file "lox-parser" :depends-on ("syntax/syntax" "lox-error"))
               (:file "interpreter" :depends-on ("lox-parser" "environment"))
               (:file "lox" :depends-on ("interpreter"))))

;; (defsystem "aoc19/tests"
;;   :depends-on ("aoc19" "rove")
;;   :components ((:file "day05-tests")
;;                (:file "intcode-tests")))
