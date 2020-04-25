(defsystem "clox"
  :depends-on ("cl-interpol" "cl-graph" "readable" "trivial-arguments"
                             "queues" "log4cl" "array-operations"
                             "alexandria" "defclass-std" "iterate"
                             "unix-opts")
  :components ((:file "clox")))

;; (defsystem "aoc19/tests"
;;   :depends-on ("aoc19" "rove")
;;   :components ((:file "day05-tests")
;;                (:file "intcode-tests")))
