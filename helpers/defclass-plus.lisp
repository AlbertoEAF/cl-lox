(defpackage :defclass+
  (:use :cl :checked-class)
  (:export :defclass+ :defclass++))
(in-package :defclass+)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun create-keyword (name)
    "Creates a keyword (upcases first) from the string."
    (intern (string-upcase name) "KEYWORD"))

  (defun with=slot-initarg (slot)
    "Add :initarg :x for slot with name x if :initarg was not set already"
    (if (getf (cdr slot) :initarg) slot
        (append slot (list :initarg (create-keyword (car slot)))))))

(defmacro defclass+ (name superclasses slots)
  "Defines type-checked class. slots get a default :initarg of the same name as the slot."
  `(locally (declare (optimize safety))
     (defclass ,name ,superclasses
       ,(mapcar #'with=slot-initarg slots)
       (:metaclass checked-class))))

(defmacro defclass++ (name superclasses slots)
  "Besides running defclass+ it creates a constructor named make-<name>."
  (let* ((constructor-name (intern (string-upcase (format nil "make-~A" name))))
         (slot-names (mapcar #'car slots))
         (make-instance-args (apply #'append (mapcar #'list
                                                     (mapcar #'create-keyword slot-names)
                                                     slot-names))))
    `(progn
       (defclass+ ,name ,superclasses ,slots)
       (defun ,constructor-name ,slot-names
         (make-instance ',name ,@make-instance-args)))))
