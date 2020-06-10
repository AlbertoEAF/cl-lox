(defpackage :defclass+
  (:use :cl :checked-class)
  (:export :defclass+ :defclass++))
(in-package :defclass+)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun create-keyword (name)
    "Creates a keyword (upcases first) from the string."
    (intern (string-upcase name) "KEYWORD"))

  (defun create-symbol (name)
    "Interns a symbol from a string or symbol."
    (intern (string-upcase name)))

  (defun add-default-prop (slot prop-name prop-value)
    "Adds a prop to a slot if it didn't exist."
    (if (getf (cdr slot) prop-name) slot
        (append slot (list prop-name prop-value))))

  (defun with=default-initarg (slot)
    "Add :initarg :x for slot with name x if :initarg was not set already"
    (add-default-prop slot :initarg (create-keyword (car slot))))

  (defun with=default-accessor (slot)
    "Add :accessor x for slot with name x if :accessor was not set already."
    (add-default-prop slot :accessor (create-symbol (car slot)))))

(defparameter *default-slot-enhancers*
  (list #'with=default-initarg)
  "Default list of slot enhancers to use.")

(defmacro defclass+ (name superclasses slots
                     &key (slot-enhancers *default-slot-enhancers*))
  "Defines type-checked class. slots get a default :initarg of the same name as the slot."
  (dolist (slot-enhancer slot-enhancers)
    (setf slots (mapcar slot-enhancer slots)))
  `(locally (declare (optimize safety))
     (defclass ,name ,superclasses
       ,slots
       (:metaclass checked-class))))

(defmacro defclass++ (name superclasses slots &key (slot-enhancers *default-slot-enhancers*))
  "Besides running defclass+ it creates a constructor named make-<name>."
  (let* ((constructor-name (intern (string-upcase (format nil "make-~A" name))))
         (slot-names (mapcar #'car slots))
         (make-instance-args (apply #'append (mapcar #'list
                                                     (mapcar #'create-keyword slot-names)
                                                     slot-names))))
    `(progn
       (defclass+ ,name ,superclasses ,slots :slot-enhancers ,slot-enhancers)
       (defun ,constructor-name ,slot-names
         (make-instance ',name ,@make-instance-args)))))
