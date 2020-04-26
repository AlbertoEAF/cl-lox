;;  https://lisp-journey.gitlab.io/blog/clos-tutorial/
;;; https://stackoverflow.com/questions/51723992/how-to-force-slots-type-to-be-checked-during-make-instance

(defpackage :checked-class
  (:use :cl))
(in-package :checked-class)

(require 'closer-mop)

;; first a metaclass for classes which checks slot writes
(defclass checked-class (standard-class)
  ())

; this is a MOP method, probably use CLOSER-MOP for a portable version
(defmethod sb-mop:validate-superclass
           ((class checked-class)
            (superclass standard-class))
   t)

(defmethod (setf sb-mop:slot-value-using-class) :before
              (new-value (class checked-class) object slot)
  (assert (typep new-value (sb-mop:slot-definition-type slot))
      ()
    "new value ~a is not of type ~a in object ~a slot ~a"
    new-value (sb-mop:slot-definition-type slot) object slot))
