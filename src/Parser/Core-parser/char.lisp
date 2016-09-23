(in-package "PARSER")
;;
;; Time-stamp: <Thu Sep 22 14:41:24 EDT 2016 jallen>


;;;; This file contains functions converting string input
;;;; into a list of word symbols

;;  TOKENIZE
;;
;;  maps a string into a list of symbolsp

(defun tokenize (inputt)
  (let ((*package* (find-package 'parser)))
    (when inputt
      (convert-to-words 
       (expand-contractions
	(coerce (string-upcase (String-trim '(#\space #\Tab #\Newline #\return) inputt)) 'list))))))


;; replaces ' with ^, and other punctuation marks with explicit keywords 

(defvar *punc-list* '((#\. :PUNC-PERIOD) 
		      (#\, :PUNC-COMMA) (#\: :punc-colon) 
		      (#\; :punc-semicolon) (#\? :punc-question-mark) 
		      (#\! :punc-exclamation-mark) (#\* :punc-times ) 
		      (#\- :PUNC-MINUS) (#\+ :PUNC-PLUS)
		      (#\( :start-paren) (#\) :end-paren)
		      (#\[ :start-square-paren) (#\] :end-square-paren)
		      (#\$ :dollar-sign) (#\| :vertical-bar)
		      (#\_ :under-bar) (#\{ :curly-left-bracket) (#\} :curly-right-bracket)
		      (#\% :Percent) (#\@ :at-sign)
		      (#\" :punc-quotemark) 
		      (#\\ :punc-backslash) (#\/ :punc-slash)
		      (#\` #\^)
		      (#\~ :punc-tilde)
		      (#\# :hashmark)
		      (#\U+2013 :punc-minus))) ;;:punc-en-dash)))

(defvar *break-chars* (cons '#\space (mapcar #'car *punc-list*)))

(defvar *punc-tokens* (mapcar #'(lambda (x) (convert-to-package (cadr x) 'w :convert-keywords t))
			      *punc-list*))

;;; Accepts strings and/or characters and forms a symbol out of them.
;;; If there are any lower case characters involved, the symbol will be
;;; surrounded by |'s.  [janet]
;;; the arguments have to be strings. (marc)
;;; Not any more.  [janet jun23]
;;; Changed to put everything in uppercase for simplicity.  jmh 07/22/90


(defun implode-to-string (x)
  ;; gives string from char.list, string list, or atom list
  (if (null x) nil
    (if (characterp (car x))
	(coerce x 'string)
      (if (stringp (car x))
	  (eval (cons 'concatenate (cons ''string x)))
	(if (atom (car x))
	    (eval (cons 'concatenate
			(cons ''string (mapcar 'string x)) )))))))


(defun convert-to-word (chars)
  (let* ((strng (implode-to-string chars))
	 (x (safe-read-from-string strng)))
    (cond ((symbolp x) (intern (symbol-name x) :W))
	  ((numberp x) strng)
	  (t x))))
  
(defun convert-to-words (charlist)
  (do ((chars charlist) (i 0) (prefix nil))
      ((null chars))
    ;; trim leading blanks
    (do () ((and (not (eql (car chars) #\Space))
		 (not (eql (car chars) #\Newline)))) (pop chars))
    (if (null chars) (return nil))
    ;; check for a quoted string
    (if (eql (car chars) :punc-quotemark)
	(let ((i (position :punc-quotemark (cdr chars))))
	  (if i
	      (setq prefix (coerce-to-string-with-special-chars (cddr (butlast chars (-  (length chars) i)))))
	    (setq prefix (convert-to-word (list (car chars))))
	    )
	  (if i
	      (setq chars (nthcdr (+ i 2) chars))
	    (setq chars (cdr chars))))
      (progn
	(setq i (or (position #\Space chars)  (position #\Newline chars))) ; locate next blank, if any
	;; copy prefix preceding next blank or end of input
	(if i (setq prefix (convert-to-word (butlast chars (- (length chars) i))))
	  (setq prefix (convert-to-word chars)))
	(if i (setq chars (nthcdr i chars)) ; trim prefix off chars
	  (setq chars nil))
	))
    ;; make upper case symbol out of prefix, make this symbol the
    ;; first element of the list of words, & finish recursively
    (return (cons prefix (convert-to-words chars))))
  )

(defun coerce-to-string-with-special-chars (charlist)
  (when charlist
    (concatenate 'string (coerce-letter-to-string (car charlist))
	       (coerce-to-string-with-special-chars (cdr charlist)))))

(defun coerce-letter-to-string (char)
  (coerce (if (symbolp char)
	      (list (or (car (find-if #'(lambda (x) (eq (second x) char)) *punc-list*))
			#\?))
	      (list char)) 'string))


;; ********* chop-last ************
;;
;; number must be 1,2, or 3
;; if number is 1, chop-last returns clist minus its last item
;; if number is 2, chop-last returns clist minus its last 2 items
;; if number is 3, chop-last returns clist minus its last 3 items
;;
(defun chop-last (number clist)
  (if (or (> number 3) 
	  (< number 1))
	  (progn (format t "Illegal option to chop-last~%")
		 '())
    (if (equal number 1)
	(reverse (cdr (reverse clist)))
      (if (equal number 2)
	  (reverse (cddr (reverse clist)))
	(if (equal number 3)
	    (reverse (cdddr (reverse clist))))))))

;; ********** safe-read-from-string
;;
;; same as read-from-string but echos nil strings
;;  instead of giving an error
;;
(defun safe-read-from-string (sstring)
  (if (null sstring) nil (read-from-string sstring nil)))


;; CONTRACTIONS -- expand out expression using single quote -- for possessives
;;     and other contractions. Outputs special lexical items ^S (john's, it's), O^ (o'clock)

(defun expand-contractions (c-list)
  (if (null c-list) nil
      (let*  ((first (car c-list))
	      (rest (cdr c-list))
	      (second (car rest))
	      (third (cadr rest))
	      (map (assoc first *punc-list*)))
	(cond 
	  (map
	   (cons #\space (cons (cadr map)
			       (cons #\space (expand-contractions (cdr c-list))))))
	  ;; special case for 10am, 4pm, 1st, 2nd, 3rd, 4th, ... -- break apart
	  ;;  also check for numbers with commas in them, e.g.,  3,000.
	 
	  ((digit-char-p first) 
	   (case (cadr c-list) 
	     ((#\A #\P)
	      (if (eql (caddr c-list) #\M)
		(cons first (cons #\space (expand-contractions (cdr c-list))))
		(cons first (cons #\space (expand-contractions (cdr c-list))))))
	   ;;   1st, 1980s
	   (#\S (cond ((and (eql first #\1) (eq (caddr c-list) #\T))
		       (cons first (list* #\space :punc-ORDINAL (expand-contractions (cdddr c-list)))))
		      ((and (eql first #\0) (or (null (cddr c-list)) (not (both-case-p (caddr c-list)))))
		       (cons first (cons #\space (cons #\^ (expand-contractions (cdr c-list))))))
		      (t (cons first (cons #\space (expand-contractions (cdr c-list)))))))
	   ;;  2nd
	   (#\N (if (and (eql first #\2) (eq (caddr c-list) #\D))
		   (cons first (list* #\space :punc-ORDINAL (expand-contractions (cdddr c-list))))
		   (cons first (cons #\space (expand-contractions (cdr c-list))))))
	   ;; 3rd
	   (#\R (if (and (eql first #\3) (eq (caddr c-list) #\D))
		   (cons first (list* #\space :punc-ORDINAL (expand-contractions (cdddr c-list))))
		   (cons first (cons #\space (expand-contractions (cdr c-list))))))
	   ;; 5th
	   (#\T (if (eq (caddr c-list) #\H)
		   (cons first (list* #\space :punc-ORDINAL (expand-contractions (cdddr c-list))))
		   (cons first (cons #\space (expand-contractions (cdr c-list))))))
	  		
	   (otherwise 
	    (if (and (eql second #\,) (caddr c-list) (digit-char-p (caddr c-list)))
		(cons first (list*  #\space :punc-comma #\space (expand-contractions (cddr c-list))))
		(if (and second (not (digit-char-p second)))
		    (cons first (cons #\space (expand-contractions (cdr c-list))))
		    (cons first (expand-contractions (cdr c-list)))))
	    )
	   ))

	
	((and (both-case-p first) second (digit-char-p second))
	 (cons first (cons #\space  (expand-contractions (cdr c-list)))))
	((eql first #\_)
	 (cons #\space (expand-contractions rest)))
	
	;;     'm, 's, 'll, 't etc
	((eql first #\')
	 (if (or (and (member second '(#\M #\S #\T #\D)) (or (null third) (eq third #\space)))
		 (and (eq second #\L) (eq third #\L))
		 (and (eq second #\R) (eq third #\E))
		 (and (eq second #\V) (eq third #\E)))
	     (cons #\space (cons #\^ (expand-contractions rest)))
	     ;; else treat a single quote mark
	     (cons #\space (cons #\^ (cons #\space (expand-contractions rest))))))

	;; n't as in didn't
	((and (eql first #\N) (equal second #\') (equal third #\T))
	 (cons #\space (cons #\N (cons #\^ (cons #\T (expand-contractions (cddr rest)))))))
	;; e.g. o'clock, o'connor, ...  
	((and (eql first #\O) (eql second #\'))
	 (if third
	     (if (both-case-p third)
		 (list* #\O #\^ third 
			(expand-contractions (nthcdr 2 rest))))
	     (list  #\O #\^)))
	 ;;#\C)
#||	      (equal (butlast (cddr rest) (- (list-length (cddr rest)) 4))
		     '(#\L #\O #\C #\K)))
	 (append '(#\O #\^  #\space #\C #\L #\O #\C #\K)
		 (expand-contractions (nthcdr 6 rest))))||#
	;; let's
	((equal (butlast c-list (max (- (list-length c-list) 5) 0))
		'(#\L #\E #\T #\' #\S))
	 (list* #\L #\E #\T #\space #\^ #\S
		(expand-contractions (nthcdr 5 c-list))))
	
	;; if there's more letters, just continue
	(second (cons first (expand-contractions rest)))
	(t c-list)))))

  
          
;; coerces a string into a list of characters with no lowercase.
(defun str-to-cap-chars (str)
  (mapcar #'char-upcase (coerce str 'list)))
