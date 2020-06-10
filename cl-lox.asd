(defsystem "cl-lox"
  :depends-on ("cl-interpol" "cl-graph" "trivial-arguments"
                             "queues" "log4cl" "array-operations"
                             "alexandria" "defclass-std" "iterate"
                             "unix-opts" "defenum" "closer-mop"
                             "defstar" "str" "rutils")
  ;; Just for debugging during dev
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3) (safety 3) (speed 0)))
                    (funcall next))
  ;;:serial t
  :components (;; Utils (can be moved to external libraries)
               (:file "helpers/checked-class")
               (:file "helpers/defclass-plus" :depends-on ("helpers/checked-class"))
               (:file "helpers/cl-extensions")
               ;; Lox code
               (:file "lox-cl" :depends-on ("helpers/cl-extensions" "helpers/defclass-plus"))
               (:file "lox-token-types" :depends-on ("lox-cl"))
               (:file "lox-token" :depends-on ("lox-token-types"))
               (:file "lox-error" :depends-on ("lox-token"))
               (:file "scanner" :depends-on ("lox-token" "lox-error"))
               (:file "syntax/syntax-expr" :depends-on ("lox-token"))
               (:file "syntax/syntax-stmt" :depends-on ("lox-token"))
               (:file "syntax/syntax" :depends-on ("syntax/syntax-expr" "syntax/syntax-stmt"))
               (:file "syntax/pprint" :depends-on ("syntax/syntax"))
               (:file "environment" :depends-on ("lox-error"))
               (:file "lox-parser" :depends-on ("syntax/syntax" "lox-error"))
               (:file "interpreter-def" :depends-on ("environment" "lox-error"))
               (:file "lox-callable" :depends-on ("interpreter-def" "environment"))
               (:file "lox-function-def" :depends-on ("lox-callable"))
               (:file "interpreter-build" :depends-on ("lox-callable" "interpreter-def" "lox-function-def"))
               (:file "lox-function" :depends-on ("lox-function-def" "interpreter-build"))
               (:file "interpreter" :depends-on ("lox-parser" "environment" "lox-callable" "lox-function" "interpreter-def"))
               (:file "resolver" :depends-on ("interpreter"))
               (:file "lox" :depends-on ("resolver"))))

;; (defsystem "aoc19/tests"
;;   :depends-on ("aoc19" "rove")
;;   :components ((:file "day05-tests")
;;                (:file "intcode-tests")))
