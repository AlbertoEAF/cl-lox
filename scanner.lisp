(defpackage :lox.scanner
  (:use :cl :defstar :checked-class :defstar :cl-extensions
        :lox.token) ;; *TODO* use local nickname!
  (:export :scanner :make-scanner :scan-tokens))
(in-package :lox.scanner)


(defvar *keywords* '(AND CLASS ELSE FALSE FOR FUN IF NIL OR PRINT RETURN SUPER THIS TRUE VAR WHILE))

(locally (declare (optimize safety))
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
    (:metaclass checked-class)))

(defun make-scanner (source)
  (make-instance 'scanner :source source))

(defmacro with-scanner-slots (scanner &body body)
  `(with-slots (source tokens start current line) ,scanner
     ,@body))

(defun* at-end-p ((scanner scanner))
  (with-scanner-slots scanner
    (>= current (length source))))

(defmethod scan-tokens ((scanner scanner))
  (with-scanner-slots scanner
    (loop while (not (at-end-p scanner))
          do
             (setf start current)
             (scan-token scanner))
    (push (make-token 'EOF "" nil line) tokens)
    (setf tokens (reverse tokens))
    tokens))

(defun* scan-token ((scanner scanner) &key (preserve-nil-token-types nil))
  (with-curry (match advance) scanner
    (let* ((c (advance))
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
               (#\! (if (match #\=) 'BANG_EQUAL 'BANG))
               (#\= (if (match #\=) 'EQUAL_EQUAL 'EQUAL))
               (#\< (if (match #\=) 'LESS_EQUAL 'LESS))
               (#\> (if (match #\=) 'GREATER_EQUAL 'GREATER))
               ;; exception: / -- can also be a comment
               (#\/
                (cond ((match #\/) (simple-comment-parser scanner))
                      ((match #\*) (nested-comment-parser scanner))
                      (t 'SLASH)))
               ;; Ignore whitespace
               ((#\Space #\Return #\Tab))
               (#\Newline (incf (line scanner)))
               ;; strings
               (#\" (lox-string scanner) nil)
               ;; No match: error
               (t
                (cond ((is-digit-p c) (lox-number scanner))
                      ((is-alpha-p c) (identifier scanner))
                      (t (lox.error::lox-error (line scanner)
                                               "Unexpected character.")))))))
      (when (and (typep token-candidate 'symbol)
                 ;; if nil, depends on preserve-nil-token-types option:
                 (or (not (null token-candidate))
                     preserve-nil-token-types))
        (add-token scanner token-candidate)))))

(defun simple-comment-parser (scanner)
  (loop while (and (not (at-end-p scanner))
                   (char/= #\Newline (peek scanner)))
        do (advance scanner)))

(defun* nested-comment-parser ((scanner scanner))
  (with-curry (match at-end-p advance) scanner
    (loop
      with nesting-level = 1
      while (and (plusp nesting-level)
                 (not (at-end-p)))
      do
         (cond ((and (match #\*)
                     (match #\/))
                (decf nesting-level))
               ((and (match #\/)
                     (match #\*))
                (incf nesting-level))
               (t (advance))))))

(defun* identifier ((scanner scanner))
  (loop while (is-alpha-numeric (peek scanner))
        do (advance scanner))
  (let ((text (subseq (source scanner)
                      (start scanner)
                      (current scanner))))
    (add-token scanner (or (car (member text *keywords* :test #'string-equal))
                           'IDENTIFIER))))

(defun* lox-number ((scanner scanner))
  (with-scanner-slots scanner
    (with-curry (peek peek-next advance) scanner
      (loop while (is-digit-p (peek))
            do (advance))
      (when (and (char= #\. (peek))
                 (is-digit-p (peek-next)))
        (advance)
        (loop while (is-digit-p (peek))
              do (advance)))
      (add-token scanner 'NUMBER (parse-number (subseq source
                                                       start
                                                       current))))))

(defun* lox-string ((scanner scanner))
  "Parse string into a token and add it to tokens"
  (with-scanner-slots scanner
    (with-curry (peek at-end-p advance) scanner
      (loop while (and (char/= #\" (peek))
                       (not (at-end-p)))
            do
               (if (char= #\Newline (peek)) (incf line)
                   (advance)))
      (when (at-end-p)
        (lox.error::lox-error line "Unterminated string.")
        (return-from lox-string nil))
      (advance) ;; consume closing \"
      (add-token scanner 'STRING (subseq source
                                         (1+ start)
                                         (1- current))))))


(defun* match ((scanner scanner) (expected standard-char))
  (cond ((at-end-p scanner) nil)
        ((char/= expected
                 (peek scanner))
         nil)
        (t (incf (current scanner)))))


(defun* peek ((scanner scanner))
  (if (at-end-p scanner) #\Nul
      (aref (source scanner) (current scanner))))

(defun* peek-next ((scanner scanner))
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

(defun* advance ((scanner scanner))
  (with-scanner-slots scanner
    (incf current)
    (aref source (1- current))))



(defun* add-token ((scanner scanner) (token-type token-type) &optional literal)
  (with-scanner-slots scanner
    (push (make-token token-type
                      (subseq source start current)
                      literal
                      line)
          tokens)))


(defun parse-number (string)
  "To do: assert return type is number"
  (with-input-from-string (input string)
    (read input)))
