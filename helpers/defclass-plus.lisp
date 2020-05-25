(defpackage :defclass+
  (:use :cl :checked-class)
  (:export :defclass+))
(in-package :defclass+)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun with=slot-initarg (slot)
    "Add :initarg :x for slot with name x if :initarg was not set already"
    (if (getf (cdr slot) :initarg) slot
        (append slot (list :initarg (intern (string-upcase (car slot)) "KEYWORD"))))))

(defmacro defclass+ (name superclasses slots)
  "Defines type-checked class. slots get a default :initarg of the same name as the slot."
  `(locally (declare (optimize safety))
     (defclass ,name ,superclasses
       ,(mapcar #'with=slot-initarg slots)
       (:metaclass checked-class))))
