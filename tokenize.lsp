;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         The Open University
;;;
;;; A Prolog lexical analyser written in Lisp. This might seem rather strange
;;; or even rather pointless, but it is a good start for the various Lisp
;;; versions of Prolog that we need. My plan is to be able to read Prolog
;;; code and convert it to some Lisp equivalent that we can run a Prolog
;;; interpreter on. 
;;;
;;; Now the question is: how are we going to use this in HANK? We could simply 
;;; use Prolog notation for everything. This would at least be flexible. But we
;;; don't need that kind of flexibility. A better strategy might simply to be 
;;; to code a standard parser for the purposes we want. This is OK with the 
;;; relatively small number of data types we care about. 

(in-package "HANK")

;;; Some tables. These are used to identify which characters are of which types
;;; a bit more efficiently than would otherwise be the case.

(defconstant letter-character 1)
(defconstant digit-character 2)
(defconstant lowercase-character 4)
(defconstant graphic-character 8)
(defconstant symbolic-control-character 16)
(defconstant solo-character 32)
(defconstant layout-character 64)
(defconstant alpha-character 128)
(defconstant octal-character 256)
(defconstant hexadecimal-character 512)

(defconstant character-table
  (make-array #x100 :element-type 'integer :initial-element 0))

(loop for code from 0 below #x100
      for char = (ascii-to-character code)
      if char
        if (alpha-char-p char)
          do (setf (aref character-table code) (+ (aref character-table code) letter-character))
          and do (setf (aref character-table code) (+ (aref character-table code) alpha-character))
          and if (lower-case-p char)
                do (setf (aref character-table code) (+ (aref character-table code) lowercase-character))
              end
        end
        and if (member char '(#\_ #\?))
              do (setf (aref character-table code) (+ (aref character-table code) alpha-character))
            end
        and if (digit-char-p char)
              do (setf (aref character-table code) (+ (aref character-table code) digit-character))
            end
        and if (digit-char-p char 8)
              do (setf (aref character-table code) (+ (aref character-table code) octal-character))
            end
        and if (digit-char-p char 16)
              do (setf (aref character-table code) (+ (aref character-table code) hexadecimal-character))
            end
        and if (member char '(#\Space #\Linefeed #\Return #\Tab #\%))
              do (setf (aref character-table code) (+ (aref character-table code) layout-character))
            end
        and if (member char '(#\# #\$ #\& #\* #\+ #\- #\. #\/ #\: #\< #\= #\> #\@ #\^ #\~))
              do (setf (aref character-table code) (+ (aref character-table code) graphic-character))
            end
        and if (member char '(#\! #\( #\) #\, #\; #\[ #\] #\{ #\} #\| #\%))
              do (setf (aref character-table code) (+ (aref character-table code) solo-character))
            end
        and if (member char '(#\a #\b #\f #\n #\r #\t #\v #\x))
              do (setf (aref character-table code) (+ (aref character-table code) symbolic-control-character))
            end)

(defmacro layout-character-p (char)
  `(logtest layout-character (aref character-table (character-to-ascii ,char))))

(defmacro graphic-character-p (char)
  `(logtest graphic-character (aref character-table (character-to-ascii ,char))))

(defmacro letterp (char)
  `(logtest alpha-character (aref character-table (character-to-ascii ,char))))

(defmacro lowercase-letter-p (char)
  `(= #.(+ letter-character lowercase-character)
      (logand (aref character-table (character-to-ascii ,char)) #.(+ letter-character lowercase-character))))

(defmacro uppercase-letter-p (char)
  `(= #.(+ letter-character)
      (logand (aref character-table (character-to-ascii ,char)) #.(+ letter-character lowercase-character))))

(defmacro alphanumeric-character-p (char)
  `(logtest #.(logior alpha-character digit-character)
            (aref character-table (character-to-ascii ,char))))

(defmacro decimal-digit-p (char)
  `(logtest digit-character (aref character-table (character-to-ascii ,char))))

(defmacro hexadecimal-digit-p (char)
  `(logtest hexadecimal-character (aref character-table (character-to-ascii ,char))))

;;; Tokens need to be accumulated relatively efficiently, character by character, until
;;; we are ready to turn them into a value. The natural for this is a string with a
;;; fill pointer. 

(defconstant token-buffer-length 1024)
(defconstant stream-buffer-length 1024)

(defstruct (tokeniser (:constructor make-tokeniser (stream)))
  (stream ())
  (stream-buffer (make-array stream-buffer-length :fill-pointer 0 :element-type 'character))
  (stream-buffer-index 0)
  (token-buffer (make-array token-buffer-length :fill-pointer 0 :element-type 'character))
  (current-character ())
  (current-token ()))

;;; This function is implementation-dependent, or could be so, for efficiency.
;;; Here I'm using a slow version for portability.

#-Common-Graphics
(defun buffer-fill-buffer (tokeniser)
  (loop with stream-buffer = (tokeniser-stream-buffer tokeniser)
        with stream = (tokeniser-stream tokeniser)
        initially (setf (fill-pointer stream-buffer) 0)
        for index from 0 below stream-buffer-length
        for char = (read-char stream () ())
        until (null char)
        do (vector-push char stream-buffer)
        finally do (setf (fill-pointer stream-buffer) index)
                   (setf (tokeniser-stream-buffer-index tokeniser) 0)
                   (return-from buffer-fill-buffer (not (zerop index)))))

#+Common-Graphics
(defun buffer-fill-buffer (tokeniser)
  (let* ((stream-buffer (tokeniser-stream-buffer tokeniser))
         (stream (tokeniser-stream tokeniser))
         (string-buffer '#.(make-string 255))
         (requested nil)
         (found nil)
         (index 0))
    (unless stream (return-from buffer-fill-buffer nil))
    (setf (fill-pointer stream-buffer) 0)
    (loop
      (setf requested (min 255 (- stream-buffer-length index)))
      (when (or (zerop requested)
                (zerop (setf found (cg:device-nread-string stream string-buffer requested))))
        (setf (fill-pointer stream-buffer) index)
        (setf (tokeniser-stream-buffer-index tokeniser) 0)
        (return-from buffer-fill-buffer (not (zerop index))))
      (loop for i from 0 below found
            do (vector-push (schar string-buffer i) stream-buffer))
      (incf index found))))
        
;;; Now we can start to look at the system of buffered reading and writing. This
;;; is independent of the code which fills the stream buffer. 
     
(defun buffer-read-char (tokeniser &optional (errorp t) &aux char index buffer)
  (cond ((setf char (tokeniser-current-character tokeniser))
         (setf (tokeniser-current-character tokeniser) (rest char))
         (first char))
        ((= (setf index (tokeniser-stream-buffer-index tokeniser))
            (fill-pointer (setf buffer (tokeniser-stream-buffer tokeniser))))
         (if (buffer-fill-buffer tokeniser)
             (buffer-read-char tokeniser)
             (when errorp
               (error "Unexpected end of file"))))
        (t
         (setf char (aref buffer index))
         (setf (tokeniser-stream-buffer-index tokeniser) (1+ index))
         char)))

(defun buffer-unread-char (tokeniser char)
  (when char
    (push char (tokeniser-current-character tokeniser))))

(defmacro buffer-keep (tokeniser char)
  `(vector-push ,char (tokeniser-token-buffer ,tokeniser)))

(defmacro buffer-clear (tokeniser)
  `(setf (fill-pointer (tokeniser-token-buffer ,tokeniser)) 0))

(defun buffer-value (tokeniser)
  (let* ((buffer (tokeniser-token-buffer tokeniser))
         (length (fill-pointer buffer))
         (string (make-string length)))
    (replace string buffer)
    string))

;;; Now, we can start to write the tokeniser. It takes a function which returns the
;;; next character or nil, to mark the end of the file. It should return two values
;;; on completion: the token type and the token value. 

(defun buffer-read-escape-sequence (tokeniser char radix &aux number)
  (unless (setf number (digit-char-p char radix))
    (error "Invalid digit in escape sequence"))
  (loop with code
        for char = (buffer-read-char tokeniser)
        if (eql char #\\)
          do (return number)
        else if (setf code (digit-char-p char radix))
          do (setf number (+ (* number radix) code))
        else
          do (error "Invalid digit in escape sequence")))

(defun buffer-read-quoted-token (tokeniser quote continuationp)
  (loop for char = (buffer-read-char tokeniser ())
        if (eql char quote)
          do (let ((char (buffer-read-char tokeniser ())))
               (if (eql char quote)
                   (buffer-keep tokeniser char)
                   (progn
                     (when char (buffer-unread-char tokeniser char))
                     (return-from buffer-read-quoted-token))))
        else if (eql char #\\)
          do (let ((char (buffer-read-char tokeniser ())))
               (cond ((null char) (error "Unexpected end of file in a quoted token"))
                     ((and continuationp (eql char #\Newline)))
                     ((eql char #\\) (buffer-keep tokeniser #\\))
                     ((eql char #\') (buffer-keep tokeniser #\'))
                     ((eql char #\") (buffer-keep tokeniser #\"))
                     ((eql char #\`) (buffer-keep tokeniser #\`))
                     ((eql char #\a) (buffer-keep tokeniser #.(ascii-to-character 7)))
                     ((eql char #\b) (buffer-keep tokeniser #\Backspace))
                     ((eql char #\f) (buffer-keep tokeniser #\Page))
                     ((eql char #\n) (buffer-keep tokeniser #\Newline))
                     ((eql char #\r) (buffer-keep tokeniser #\Return))
                     ((eql char #\t) (buffer-keep tokeniser #\Tab))
                     ((eql char #\v) (buffer-keep tokeniser #\Linefeed))
                     ((eql char #\x)
                      (let ((char (buffer-read-char tokeniser ())))
                        (if char
                            (buffer-keep tokeniser 
                              (ascii-to-character (buffer-read-escape-sequence tokeniser char 16)))
                            (error "Unexpected end of file in a quoted token"))))
                     (t
                      (buffer-keep tokeniser 
                        (ascii-to-character (buffer-read-escape-sequence tokeniser char 8))))))
         else if char
           do (buffer-keep tokeniser char)
         else
           do (error "Unexpected end of file in a quoted token")))

;;; We'll start at the top of the lexical analyser, then, with the code which
;;; reads a token. We need to take some care with this, to ensure that tokens
;;; are properly separated. Layout text is whitespace and comments, which need 
;;; to be handled by being recognised. Therefore, we need to skip layout, 
;;; and to return t if any characters were skipped. This we can use to decide
;;; on various other tokens.

(defun skip-layout-p (tokeniser)
  (loop with readp = nil
        for char = (buffer-read-char tokeniser ())
        if (null char)
          do (return readp)
        else if (eql char #\%)
          do (loop for char = (buffer-read-char tokeniser)
                   until (eql char #\Newline))
             (setf readp t)
        else if (eql char #\/)
          do (let ((char (buffer-read-char tokeniser ())))
               (if (and char (eql char #\*))
                   (loop with read* = nil
                         for char = (buffer-read-char tokeniser)
                         if (eql char #\*)
                           do (setf read* t)
                         else if (and read* (eql char #\/))
                           do (return ())
                         else
                           do (setf read* nil))
                   (progn
                     (when char (buffer-unread-char tokeniser char))
                     (buffer-unread-char tokeniser #\/)
                     (return readp))))
             (setf readp t)
        else if (layout-character-p char)
          do (setf readp t)
        else
          do (return (progn
                       (buffer-unread-char tokeniser char)
                       readp))))

(defun buffer-unread-token (tokeniser &rest arguments)
  (setf (tokeniser-current-token tokeniser) arguments))

(defun buffer-read-token (tokeniser)
  (let ((token (tokeniser-current-token tokeniser)))
    (when token
      (setf (tokeniser-current-token tokeniser) ())
      (return-from buffer-read-token (apply #'values token))))
  (skip-layout-p tokeniser)
  (buffer-clear tokeniser)
  (let ((char (buffer-read-char tokeniser nil))
        (buffer (tokeniser-token-buffer tokeniser))
        (token ()))
    (unless char
      (return-from buffer-read-token nil))
    (cond ((setf token (assoc char '((#\( . open)
                                     (#\) . close)
                                     (#\[ . open-list)
                                     (#\] . close-list)
                                     (#\| . head-tail)
                                     (#\, . comma))))
           (values (rest token) (string char)))
          ((or (eql char #\\)
               (graphic-character-p char))
           (buffer-keep tokeniser char)
           (loop for char = (buffer-read-char tokeniser ())
                 if (and char
                         (or (eql char #\\)
                             (graphic-character-p char)))
                   do (buffer-keep tokeniser char)
                 else
                   do (return (progn
                                (when char (buffer-unread-char tokeniser char))
                                (if (and (or (null char) (layout-character-p char))
                                         (= (fill-pointer buffer) 1)
                                         (eql (char buffer 0) #\.))
                                    (values 'end ".")
                                    (values 'name (buffer-value tokeniser)))))))
          ((letterp char)
           (buffer-keep tokeniser char)
           (loop for char = (buffer-read-char tokeniser nil)
                 if (and char (alphanumeric-character-p char))
                   do (buffer-keep tokeniser char)
                 else
                   do (return (progn
                                (when char (buffer-unread-char tokeniser char))
                                (values 'name (buffer-value tokeniser))))))
          ((eql char #\')
           (buffer-read-quoted-token tokeniser char t)
           (values 'name (buffer-value tokeniser)))
          ((eql char #\")
           (buffer-read-quoted-token tokeniser char ())
           (values 'char-code-list 
                   (loop for char across buffer
                         collect (character-to-ascii char))))
          ((decimal-digit-p char)
           (loop with number = (digit-char-p char 10)
                 for char = (buffer-read-char tokeniser ())
                 if (and char (decimal-digit-p char))
                   do (setf number (+ (* number 10) (digit-char-p char 10)))
                 else if (and (eql char #\x)
                              (= 1 (fill-pointer buffer))
                              (eql (char buffer 0) #\0))
                   do (buffer-clear tokeniser)
                      (loop with number = 0
                            with readp = nil
                            for char = (buffer-read-char tokeniser)
                            if (hexadecimal-digit-p char)
                              do (setf number (+ (* number 16) (digit-char-p char 16)))
                                 (setf readp t)
                            else
                              do (return (progn
                                           (buffer-unread-char tokeniser char)
                                           (if readp
                                               (values 'integer number)
                                               (progn
                                                 (buffer-unread-char tokeniser #\x)
                                                 (values 'integer 0))))))
                 else if (eql char #\.)
                   do (return (let ((fraction (loop with value = 0
                                                    with divisor = 1
                                                    with readp = nil
                                                    for char = (buffer-read-char tokeniser ())
                                                    if (and char (decimal-digit-p char))
                                                      do (setf readp t)
                                                         (setf value (+ (* value 10) (digit-char-p char)))
                                                         (setf divisor (* 10 divisor))
                                                    else
                                                      do (return (progn
                                                                   (when char
                                                                     (buffer-unread-char tokeniser char))
                                                                   (if readp
                                                                       (float (/ value divisor))
                                                                       (progn
                                                                         (buffer-unread-char tokeniser #\.)
                                                                         (return-from buffer-read-token (values 'integer number))))))))
                                    (char (buffer-read-char tokeniser ())))
                                (setf number (+ number fraction))
                                (if (and char (or (eql char #\E) (eql char #\e)))
                                    (let ((sign (buffer-read-char tokeniser ())))
                                      (cond ((and sign (decimal-digit-p sign))
                                             (buffer-unread-char tokeniser sign)
                                             (setf sign ()))
                                            ((eql sign #'+))
                                            ((eql sign #\-))
                                            (t
                                             (buffer-unread-char tokeniser sign)
                                             (buffer-unread-char tokeniser char)
                                             (return-from buffer-read-token (values 'float number))))
                                      (values 'float 
                                        (* number
                                           (loop with number = 0
                                                 with readp = nil
                                                 for digit = (buffer-read-char tokeniser ())
                                                 if (and digit (decimal-digit-p digit))
                                                   do (setf readp t)
                                                      (setf number (+ (* number 10) (digit-char-p digit 10)))
                                                else
                                                   do (return (progn
                                                                (when digit (buffer-unread-char tokeniser digit))
                                                                (if readp
                                                                    (expt 10 (if (eql sign #\-)
                                                                                 (- number)
                                                                                 number))
                                                                    (progn
                                                                      (when sign (buffer-unread-char tokeniser sign))
                                                                      (buffer-unread-char tokeniser char)
                                                                      (return-from buffer-read-token (values 'float number))))))))))
                                     (progn    
                                       (when char (buffer-unread-char tokeniser char))
                                       (values 'float number)))))
                 else 
                   do (return (progn
                                (buffer-unread-char tokeniser char)
                                (values 'integer number))))))))

;;; Now we need code to read and to print (and probably pretty print) data stuff into
;;; and out of streams. This will look a bit like Prolog, but not completely. This is
;;; mainly because we will never use operators. 

(defstruct (compound (:constructor make-compound (operator arguments)))
  (operator () :read-only t)
  (arguments () :read-only t))

(defun buffer-read-term (tokeniser expressionp eof-error-p eof-value
                         &aux class value term-value name list)
  (declare (ftype (function (t t) t t t) get-operator))
  (setf term-value '#1=#:error)

  (loop
    (tagbody
      begin
      (multiple-value-setq (class value) (buffer-read-token tokeniser))
      
      (when (null class)
        (if eof-error-p
            (error "End of file")
            (return-from buffer-read-term eof-value)))

      integer
      (unless (eq class 'integer) (go float))
      (go return-value)
      
      float
      (unless (eq class 'float) (go name))
      (go return-value)
       
      return-value
      (setf term-value value)
      (go continue)
       
      name
      (unless (eq class 'name) (go open))
      (setf name value)
      (unless (string= name "\-") (go more-name))
       
      (multiple-value-setq (class value) (buffer-read-token tokeniser))
      (when (eq class 'integer) (go negate))
      (when (eq class 'float) (go negate))
      (buffer-unread-token tokeniser class value)
      (setf class 'name)
       
      more-name
      (setf term-value name)
      (setq class (buffer-read-char tokeniser nil))
      (when (eql class #\() (go continue-function))
      (when class (buffer-unread-char tokeniser class))
      (go continue)
       
      open
      (unless (eq class 'open) (go open-list))
      (setf term-value (buffer-read-term tokeniser nil t ()))
      (multiple-value-setq (class value) (buffer-read-token tokeniser))
      (unless (eq class 'close) (error "Missing close parenthesis in bracketed expression"))
      (go continue)
       
      open-list
      (unless (eq class 'open-list) (go somewhere))
      (setf list nil)
       
      read-list
      (multiple-value-setq (class value) (buffer-read-token tokeniser))
      (when (eq class 'close-list) (go end-list))
      (buffer-unread-token tokeniser class value)
      
      read-list-value
      (setf term-value (buffer-read-term tokeniser t t nil))
      (push term-value list)
      (multiple-value-setq (class value) (buffer-read-token tokeniser))
      (when (eq class 'comma) (go read-list-value))
      (when (eq class 'head-tail) (go head-tail))
      (when (eq class 'close-list) (go end-list))
      (error "Syntax error in list: unexpected token class ~A" class)
       
      end-list
      (setf term-value (nreverse list))
      (go continue)
       
      head-tail
      (setf term-value (buffer-read-term tokeniser t t nil))
      (multiple-value-setq (class value) (buffer-read-token tokeniser))
      (unless (eq class 'close-list)
        (error "Syntax error in list: missing close bracket"))
       
      (setf list (nreverse list))
      (setf (rest (last list)) term-value)
      (setf term-value list)
      (go continue)
       
      somewhere
      (error "Somewhere: unexpected token class ~A" class)
      
      negate
      (setf value (- value))
      (setf term-value value)
      (go continue)
       
      continue-function
      (setf list nil)
      
      read-function-value
      (setf term-value (buffer-read-term tokeniser t t nil))
      (push term-value list)
      (multiple-value-setq (class value) (buffer-read-token tokeniser))
      (when (eq class 'comma) (go read-function-value))
      (when (eq class 'close) (go end-function))
      (error "Syntax error in list: unexpected token class ~A" class)
       
      end-function
      (if (and (string= name ".") list (rest list) (null (rest (rest list))))
          (setf term-value (cons (second list) (first list)))
          (setf term-value (make-compound name (nreverse list))))
      (go continue)
       
      continue
      (multiple-value-setq (class value) (buffer-read-token tokeniser))
      (when (null class) (go continue-value))
      (go continue-exit)
       
      continue-value
      (return-from buffer-read-term term-value)
       
      continue-exit
      (buffer-unread-token tokeniser class value)
      (return-from buffer-read-term term-value)
      
      )))

;;; Code for writing terms. We should be able to adapt this to map to strings
;;; directly for some purposes. All this needs to be treated with care. 

(defconstant term-buffer-length 1024)

(defstruct (termer (:constructor make-termer (stream)))
  (stream ())
  (term-buffer-index 0)
  (term-buffer (make-string term-buffer-length)))

(defun buffer-write-char (termer char)
  (when (eql (termer-term-buffer-index termer) term-buffer-length)
    (buffer-write-buffer termer))
  (setf (schar (termer-term-buffer termer) (termer-term-buffer-index termer)) char)
  (incf (termer-term-buffer-index termer)))

(defun buffer-write-string (termer string)
  (loop for char across (the string string)
        do (buffer-write-char termer char)))

(defun buffer-write-buffer (termer)
  (write-string (termer-term-buffer termer) (termer-stream termer) :end (termer-term-buffer-index termer))
  (setf (termer-term-buffer-index termer) 0))

(defvar *write-term-depth* 0)

(defun buffer-write-term* (termer term pretty escape)
  (let ((*write-term-depth* (1+ *write-term-depth*)))
    (buffer-write-term1 termer term pretty escape)))

(defun buffer-write-term (termer term &key pretty escape)
  (buffer-write-term* termer term pretty escape))

(defun buffer-write-term1 (termer term pretty escape)
  (cond ((null term)
         (buffer-write-char termer #\[)
         (buffer-write-char termer #\]))
        ((compound-p term)
         (let ((operator (compound-operator term))
               (arguments (compound-arguments term)))
           (buffer-write-term* termer operator pretty escape)
           (buffer-write-char termer #\()
           (loop
             (unless arguments
               (buffer-write-char termer #\))
               (return))
             (buffer-write-term* termer (first arguments) pretty escape)
             (when (rest arguments)
               (buffer-write-char termer #\,)
               (if pretty
                   (progn
                     (buffer-write-char termer #\Newline)
                     (dotimes (i (* 2 *write-term-depth*))
                       (buffer-write-char termer #\Space)))
                   (buffer-write-char termer #\Space)))
             (pop arguments))))
        ((consp term)
         (buffer-write-char termer #\[)
         (loop
           (unless term
             (buffer-write-char termer #\])
             (return))
           (unless (consp term)
             (buffer-write-char termer #\|)
             (buffer-write-char termer #\Space)
             (buffer-write-term* termer term pretty escape)
             (buffer-write-char termer #\])
             (return))
           (buffer-write-term* termer (first term) pretty escape)
           (when (rest term)
             (when (consp (rest term))
               (buffer-write-char termer #\,))
             (buffer-write-char termer #\Space))
           (pop term)))
        ((numberp term)
         (let ((string (prin1-to-string term)))
           (loop for char across string
                 do (buffer-write-char termer char))))
        ((stringp term)
         (let ((needs-quoting-p (and escape
                                    (or (pro:izerop (length term))
                                        (not (and (letterp (char term 0))
                                                  (loop for index from 1 below (length term)
                                                         unless (alphanumeric-character-p (char term index))
                                                          do (return nil)
                                                         end
                                                         finally do (return t))))))))
           (when needs-quoting-p
             (buffer-write-char termer #\'))
           (loop for char across term
                 do (when (and needs-quoting-p (member char '(#\\ #\" #\' #\`)))
                      (buffer-write-char termer #\\))
                    (buffer-write-char termer char))
           (when needs-quoting-p
             (buffer-write-char termer #\'))))))

(defun write-term-to-string (term &key (escape nil) (pretty nil))
  (assert (not (symbolp term)))
  (let ((termer #.(make-termer ())))
    (setf (termer-term-buffer-index termer) 0)
    (buffer-write-term* termer term pretty escape)
    (subseq (termer-term-buffer termer) 0 (termer-term-buffer-index termer))))

(defun read-value-from-string (string &optional (eofp t) eof-value)
  (let* ((tokeniser '#.(make-tokeniser nil))
         (stream-buffer (tokeniser-stream-buffer tokeniser)))
    (setf (fill-pointer stream-buffer) (length string))
    (setf (tokeniser-stream-buffer-index tokeniser) 0)
    (setf (tokeniser-current-token tokeniser) nil)
    (setf (tokeniser-current-character tokeniser) nil)
    (setf (fill-pointer (tokeniser-token-buffer tokeniser)) 0)
    (replace stream-buffer string)
    (multiple-value-bind (class value) (buffer-read-token tokeniser)
      (when (tokeniser-current-character tokeniser)
        (decf (tokeniser-stream-buffer-index tokeniser)))
      (values class value (tokeniser-stream-buffer-index tokeniser)))))
