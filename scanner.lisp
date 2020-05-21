(defpackage :clox.scanner
  (:use :cl :checked-class :clox.token :defstar)
  (:export :scanner :make-scanner :scan-tokens))
(in-package :clox.scanner)


(defvar *keywords* '(AND CLASS ELSE FALSE FOR FUN IF NIL OR PRINT RETURN SUPER THIS TRUE VAR WHILE))

(defmacro let-curry (obj (&rest functions) &body body)
  "Locally curry all functions"
  `(flet ,(loop for fn in functions
                collect `(,fn (&rest args)
                              (apply #',fn ,obj args)))
     ,@body))

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
    (setf tokens (reverse tokens))
    tokens))

(defun* scan-token ((scanner scanner))
  (let* ((c (advance scanner))
         (token-candidate
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
             (#\! (if (match scanner #\=) 'BANG_EQUAL 'BANG))
             (#\= (if (match scanner #\=) 'EQUAL_EQUAL 'EQUAL))
             (#\< (if (match scanner #\=) 'LESS_EQUAL 'LESS))
             (#\> (if (match scanner #\=) 'GREATER_EQUAL 'GREATER))
             ;; exception: / -- can also be a comment
             (#\/
              (if (match scanner #\/) (loop while (and (char/= #\Newline (peek scanner))
                                                       (not (at-end-p scanner)))
                                            do (advance scanner))
                  'SLASH))
             ;; Ignore whitespace
             ((#\Space #\Return #\Tab))
             (#\Newline (incf (line scanner)))
             ;; strings
             (#\" (clox-string scanner) nil)
             ;; No match: error
             (t
              (cond ((is-digit-p c) (clox-number scanner))
                    ((is-alpha-p c) (identifier scanner))
                    (t (clox.error::clox-error (line scanner)
                                               "Unexpected character.")))))))
    (when (typep token-candidate 'symbol)
      (add-token scanner token-candidate))))

(defun identifier(scanner)
  (loop while (is-alpha-numeric (peek scanner))
        do (advance scanner))
  (let ((text (subseq (source scanner)
                      (start scanner)
                      (current scanner))))
    (add-token scanner (or (member text *keywords* :test #'string-equal)
                           'IDENTIFIER))))

(defun clox-number (scanner)
  (declare (type scanner scanner))
  (loop while (is-digit-p (peek scanner))
        do (advance scanner))
  (when (and (char= #\. (peek scanner))
             (is-digit-p (peek-next scanner)))
    (advance scanner)
    (loop while (is-digit-p (peek scanner))
          do (advance scanner)))
  (add-token scanner 'NUMBER (parse-number (subseq (source scanner)
                                                   (start scanner)
                                                   (current scanner)))))

(defun clox-string (scanner)
  "Parse string into a token and add it to tokens"
  (let-curry scanner (peek at-end-p advance source start current)
    (loop while (and (char/= #\" (peek))
                     (not (at-end-p)))
          do
             (if (char= #\Newline (peek)) (incf (line scanner))
                 (advance)))
    (when (at-end-p)
      (clox.error::clox-error (line scanner) "Unterminated string.")
      (return-from clox-string nil))
    (advance) ;; consume closing \"
    (add-token scanner 'STRING (subseq (source)
                                       (1+ (start))
                                       (1- (current))))))


(defun* match ((scanner scanner) (expected standard-char))
  (cond ((at-end-p scanner) nil)
        ((char/= expected
                 (aref (source scanner) (current scanner)))
         nil)
        (t (incf (current scanner))
           t)))

(defun* peek ((scanner scanner))
  (if (at-end-p scanner) #\Nul
      (aref (source scanner) (current scanner))))

(defun peek-next (scanner)
  (with-slots (current source) scanner
    (if (>= (1+ current)
            (length source))
        #\Nul
        (aref source (1+ current)))))

(defun is-alpha-numeric (c)
  (or (is-alpha-p c)
      (is-digit-p c)))

(defun is-alpha-p (c)
  (or (char<= #\a c #\z)
      (char<= #\A c #\Z)
      (char=  #\_ c)))


(defun is-digit-p (c)
  (char<= #\0 c #\9))

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


(defun parse-number (string)
  "To do: assert return type is number"
  (with-input-from-string (input string)
    (read input)))
