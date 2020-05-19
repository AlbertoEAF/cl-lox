(defpackage :clox.scanner
  (:use :cl :checked-class :clox.token :defstar))
(in-package :clox.scanner)


(defclass scanner ()
  ((source
    :initarg :source
    :accessor source
    :type string)
   (tokens
    :initform nil
    :accessor tokens
    :type list)
   (start
    :initform 0
    :accessor start
    :type integer)
   (current
    :initform 0
    :accessor current
    :type integer)
   (line
    :initform 1
    :accessor line
    :type integer))
  (:metaclass checked-class::checked-class))

(defun make-scanner (source)
  (make-instance 'scanner :source source))

;; (defun delimiterp (c) (or (char= c #\Space) (char= c #\Newline)))

;; (defun my-split (string &key (delimiterp #'delimiterp))
;;   (loop :for beg = (position-if-not delimiterp string)
;;           :then (position-if-not delimiterp string :start (1+ end))
;;         :for end = (and beg (position-if delimiterp string :start beg))
;;         :when beg :collect (subseq string beg end)
;;           :while end))

(defun at-end-p (scanner)
  (declare (type scanner scanner))
  (>= (current scanner) (length (source scanner))))



(defmethod scan-tokens ((scanner scanner))
  (with-slots (start current line tokens) scanner
    (loop while (not (at-end-p scanner))
          do
             (setf start current)
             (scan-token scanner))
    (push (make-token 'EOF "" nil line) tokens)
    tokens))

(defun* scan-token ((scanner scanner))
  (let ((c (advance scanner)))
    (add-token scanner
               (case c
                 ;; 1-character cases
                 (#\( 'LEFT_PAREN)
                 (#\) 'RIGHT_PAREN)
                 (#\{ 'LEFT_BRACE)
                 (#\} 'RIGHT_BRACE)
                 (#\, 'COMMA)
                 (#\. 'DOT)
                 (#\- 'MINUS)
                 (#\+ 'PLUS)
                 (#\; 'SEMICOLON)
                 (#\* 'STAR)
                 ;; 2-character cases
                 (#\! (if (match #\=) 'BANG_EQUAL 'BANG))
                 (#\= (if (match #\=) 'EQUAL_EQUAL 'EQUAL))
                 (#\< (if (match #\=) 'LESS_EQUAL 'LESS))
                 (#\> (if (match #\=) 'GREATER_EQUAL 'GREATER))
                 ;; exception: / -- can also be a comment
                 (#\/ (if (match #\/) (loop while (and (char/= (peek scanner))
                                                       (not (at-end-p scanner))))))
                 (t (clox.error::error (line scanner) "Unexpected character."))))))     

(defun* match ((scanner scanner) (expected standard-char))
  (cond ((at-end-p scanner) nil)
        ((char/= (aref (source scanner) (current scanner))
                 expected)
         nil)
        (t (incf (current scanner))
           t)))

(defun advance (scanner)
  (declare (type scanner scanner))
  (incf (current scanner))
  (aref (source scanner) (1- (current scanner))))

(defun add-token (scanner token-type &optional literal)
  (declare (type scanner scanner)
           (type token-type token-type))
  (with-slots (start current line source) scanner
    (push (make-token token-type
                      (subseq source start current)
                      literal
                      line)
          (tokens scanner))))



(defmacro args (args &body body)
  `(loop for arg in args do (print arg)))

