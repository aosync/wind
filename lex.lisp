(in-package #:wind.frontend)

(defstruct token
  "Lexically meaningful unit of code text."
  type
  text
  position)

(defmacro scase (pred &body a)
  "CASE-like macro for strings."
  (append '(block nil)
	  (list (append `(let ((pred ,pred)))
			(loop :for x :in a
			      :collect `(when (string= pred ,(car x))
					  (return ,(car (cdr x)))))))))  

(defmacro first-of (&body a)
  "Evaluate the forms in sequence, stop and return the first one which evaluates to true."
  (append '(block nil)
	  (loop :for x :in a
		:collect `(let ((value ,x))
			    (when value (return value))))))

(defmacro try-lex (self &body body)
  "Call check on the CHARBUF, execute all the forms in sequence. If the value of the last form is truthy, create a TOKEN with it as type, and VALIDATE the CHARBUF; otherwise, return NIL and rewind the CHARBUF."
  `(progn
     (check ,self)
     (let ((token (block nil ,@body)))
       (if token
	   (let ((text (region ,self))
		 (pos  (start-position ,self)))
	     (validate ,self)
	     (make-token
	      :type token
	      :text text
	      :position pos))
	   (progn
	     (rewind ,self)
	     nil)))))

(defun word-start-char-p (char)
  "Is the character a suitable word start?"
  (alpha-char-p char))

(defun word-char-p (char)
  "Is the character suitable to make up words?"
  (or (word-start-char-p char)
      (digit-char-p char)))

(defun whitespace-char-p (char)
  "Is the character whitespace?"
  (find char
	'(#\space #\tab #\linefeed #\return #\newline)))

(defun lex-whitespace (buf)
  "Lex any number of whitespace from a CHARBUF into a TOKEN."
  (try-lex buf
    (when (whitespace-char-p (peek buf))
      (loop :while (whitespace-char-p (peek buf))
	    :do (next buf))
      :whitespace)))

(defun lex-word (buf)
  "Lex a word TOKEN from a CHARBUF."
  (try-lex buf
    (when (word-start-char-p (peek buf))
      (loop :while (word-char-p (peek buf))
	    :do (next buf))
      :word)))

(defun lex-number (buf)
  "Lex a number TOKEN from a CHARBUF."
  (try-lex buf
    (when (digit-char-p (peek buf))
      (loop :while (digit-char-p (peek buf))
	    :do (next buf))
      :number)))

(defun lex-comment (buf)
  "Lex a comment TOKEN from a CHARBUF."
  (try-lex buf
    (let* ((p (next buf))
	   (q (next buf)))
      (when (and (eq p #\/)
		 (eq q #\*))
	(loop :until (and (eq p #\*)
			  (eq q #\/))
	      :do (progn
		    (setf p q)
		    (setf q (next buf))))
	:comment))))

(defun lex-symbol3 (buf)
  "Lex a three characters symbol TOKEN from a TOKBUF."
  (try-lex buf
    (next buf)
    (next buf)
    (next buf)
    (scase (region buf)
      (">>=" :rshiftas)
      ("<<=" :lshiftas)
      ("&&=" :landas)
      ("||=" :loras))))

(defun lex-symbol2 (buf)
  "Lex a two characters symbol TOKEN from a TOKBUF."
  (try-lex buf
    (next buf)
    (next buf)
    (scase (region buf)
      ("==" :equal)
      (">=" :ge)
      ("<=" :le)
      ("&&" :land)
      ("||" :lor)
      ("+=" :addas)
      ("-=" :subas)
      ("*=" :mulas)
      ("/=" :divas)
      ("%=" :modas)
      ("|=" :boras)
      ("&=" :bandas)
      ("^=" :xoras)
      ("<<" :lshift)
      (">>" :rshift))))

(defun lex-symbol1 (buf)
  "Lex a one character symbol TOKEN from a TOKBUF."
  (try-lex buf
    (next buf)
    (scase (region buf)
      ("." :dot)
      ("," :coma)
      (";" :semicolon)
      ("|" :bor)
      ("&" :band)
      ("+" :add)
      ("-" :sub)
      ("*" :mul)
      ("/" :div)
      ("%" :mod)
      ("{" :lbrace)
      ("}" :rbrace)
      ("(" :lparen)
      (")" :rparen)
      ("[" :lbracket)
      ("]" :rbracket))))

(defun lex-any (buf &key exclude)
  "Lex the next TOKEN from the TOKBUF. TOKEN types specified in exclude are excluded."
  (loop :for token = (first-of
		       (lex-whitespace buf)
		       (lex-comment buf)
		       (lex-word buf)
		       (lex-number buf)
		       (lex-symbol3 buf)
		       (lex-symbol2 buf)
		       (lex-symbol1 buf))
	:when (not (find (token-type token) exclude))
	  :do (return token)))

;; TESTS

(defparameter my
  (make-instance 'charbuf
		 :next-op #'read-char))

(loop :while t
      :do (format t "~a~%" (lex-any my :exclude '(:comment :whitespace))))
