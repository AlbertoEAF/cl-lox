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
              (if (is-digit-p c) (number scanner)
                  (clox.error::clox-error (line scanner)
                                          "Unexpected character."))))))
    (when (typep token-candidate 'symbol)
      (add-token scanner token-candidate))))

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
  (loop while (and (char/= #\" (peek scanner))
                   (not (at-end-p scanner)))
        do
           (if (char= #\Newline (peek scanner)) (incf (line scanner))
               (advance scanner)))
  (when (at-end-p scanner)
    (clox.error::clox-error (line scanner) "Unterminated string.")
    (return-from clox-string nil))
  (advance scanner) ;; consume closing "
  (add-token scanner 'STRING (subseq (source scanner)
                                     (1+ (start scanner))
                                     (1- (current scanner)))))

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



(defmacro args (args &body body)
  `(loop for arg in args do (print arg)))

(defun parse-number (string)
  "To do: assert return type is number"
  (with-input-from-string (input string)
    (read input)))
