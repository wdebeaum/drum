(in-package parser)
;;Time-stamp: <Mon Mar 22 15:54:11 EST 1999 ferguson>

;; quick hack to deal with spelling issues for typed input
;; Brad Miller (miller@cs.rochester.edu) December 1995 for the ToyTrains Project

;; Usage:

;; 1) call (reset-wordlist) if running under auspices of DM, otherwise
;;         (reset-wordlist word1 word2 word3 ...)
;;
;; The words will become the list of cities (the dictionary). The words should be
;; symbols

;; 2) (speeling-corrector word) (again word is a symbol) returns the best match
;;     in the dictionary, or the word itself if the best match is below a threshold
;;
;;    (speeling-corrector word threshold) lets you specify the threshold (normally 100)
;;
;; Scoring:
;;       A direct match for the word in the dictionary gives a score of 1000.
;;       From this arbitary number, the "cost" of changing the word to an entry
;;       in the dictionary is deducted through one or more actions. These costs
;;       are set in the below parameters, and may be adjusted by the end-user (even
;;       at runtime: since these are specials, they may be LET around the call to 
;;       speeling-corrector). 

;;       The values below are arbitrary, but produced reasonable results from a number
;;       of trial common mispellings of cities in the ToyTrains corpus. 
;;       (northeast and western new york scenarios). Since our experiments have been
;;       mostly verbal, the misspellings were mostly generated from my own typos and
;;       generations of likely misspellings (because they are documented to be common
;;       in other spelling correctors), e.g. insertling an lettter, or skpping one.
;;       Trasnpositions, rotaiotns (usually caused by hands getting out of sync when
;;       touch typing), and the simpler transopsition have different costs.
;;
;;       Typeos have two costs: small if a single character typo, larger if multi
;;       character. I.e. if the target word is "frozzbozz" "frizzbozz" would have
;;       a cost of *typo-cost* but "friqzbozz" would have a cost of *bad-typo-cost* for
;;       the first character, and *typo-cost* for the second.
;;
;;       Better results would probably be obtained if we used a model of the keyboard and
;;       made typos of "near misses" cheaper than "far misses", e.g. typing v instead of b
;;       (on a querty keyboard) would have a lower cost than p instead of a).. the former
;;       normally being pressed with the same finger, the latter requiring different hands.
;;
;;       This presupposes, of course, most typos are of the touch-type variety, the hunt and 
;;       peck typists likely err in a more graphic fashion, e.g. striking a key adjacent
;;       or mischording (striking two adjacent keys), or striking the wrong key with a visually
;;       similar glyph (0/O or i/j?). This is pure speculation on my part; the mischording
;;       certainly occasionally comes up when I type, esp on a keyboard I don't usually use
;;       or when typing numbers (and I'm a touch typist), because I don't type numbers very
;;       often...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STUFF TO INTERFACE WITH THE REST OF THE PARSER

(defparameter *min-word-length* 3 "We will not spell check words with fewer than
this many characters")
(defparameter *threshold* 900 "How 'off' can the correction be from the original?  1000= perfect match")

;; top-level function called from the robust parsing mechanism.
;;
;; 'words' is a list of symbols
;; note that "(reset-wordlist <lexicon>)" must be invoked before
;; trying to spell check any word.  Currently is done in parser.lisp
;;
;; Note we do not try to correct words which are in the lexicon, are
;; numbers, or are less than 3 chars long.
;;
;; Note also that the second arg to speeling-corrector indicates the
;; threshold for how "off" we will accept a correction (exact match is
;; 1000).
(defun spell-check (words)
  (mapcar #'(lambda (w)
	      (if (or (gethash w (get-lexicon))
		      (numberp w)
		      (< (length (symbol-name w)) *min-word-length*))
		  w
		(speeling-corrector w *threshold*)))
	  words))


;; does just what it says.
;; requires that "./parser/noun-lex.lisp" be loaded previously
;; to define the variable.
;; Note that we take the union so we get all three: "new" "york" and "new_york".

(defun init-spellcheck-list nil
  (let ( (temp (defined-words)))
    (setq temp (remove-if 
		(lambda (x) 
		  (or (numberp x) 
		      (consp x)
		      (< (length (symbol-name x)) *min-word-length*)))
		temp))
    (reset-wordlist
     (union temp
	  (reduce-to-single-words
	    temp
	   )))))




;; function used to convert words like "new_york" to "new" "york".
;; 
;; accepts and returns a list of symbols.
;;
;; no numbers allowed, but will handle "_new_york" same as "new_york"
;; by virtue of the "remove-if null".
(defun reduce-to-single-words (wordlist)
  (apply 'append
	 (mapcar #'(lambda (w)
		     (mapcar 'read-from-string
			     (remove-if 'null (split-on-char (string w) #\_))))
		 wordlist)))

;; accepts a string and a character (the third argument stores the temp
;; value needed through the recursion).  Returns a list of strings, which
;; have been delimited by 'ch'.
(defun split-on-char (str ch &optional (thisconstit nil))
  (cond ((string= str "")
	 (list thisconstit))
	((eql (char str 0) ch)
	 (cons thisconstit
	       (split-on-char (subseq str 1)
			      ch
			      nil)))
	(t
	 (split-on-char (subseq str 1)
			ch
			(concatenate 'string 
			  thisconstit
			  (string (char str 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACTUAL SPELL-CHECKING STUFF
;;


(defparameter *cost-matrix*
    (list 
     (list 'ins 30 100 10)
     (list 'del 30 100 10)
     (list 'typo 20 80 10))
  "The minimum, maximum, and 'rate' costs, respectively, for each error type")

(defun min-cost (x) (second x))
(defun max-cost (x) (third x))
(defun rate-cost (x) (fourth x))

(defvar *insert-cost* "cost to insert a char will be a function on input length")

(defvar *delete-cost* "cost to delete a char will be a function of the input length")

(defparameter *simple-transpose-cost* 25 "points deducted from score for a simple transposition")

(defvar *typo-cost* "cost of a typo (when the following char  is correct) will be rebound
 to be a function of the input length")

(defparameter *bad-typo-cost* 100 "Points deducted to change a char into another, when following char is NOT ok")

(defparameter *rotation-cost* 10 "Points deducted to rotate letters, per size of rotation")

;; this is intended for adjusting the insertion/deletion costs as a function of the length
;; of the word we are checking.  A cost will range from 100 (for 3 letter words), and gets
;; smaller by 10s as the words gets longer.  It caps out at 30, though.
(defun adjust-cost (error length)
  (max (- (max-cost (assoc error *cost-matrix*))
	  (* (rate-cost (assoc error *cost-matrix*))
	     (- length *min-word-length*)))
       (min-cost (assoc error *cost-matrix*))))


;; Testing function.
;; decide what you want
(defparameter *typos*
    ;; the first group are the cities from the TRAINS-95 evaluation
    '((cinncin . cincinnati)
      (Milwaulkee . Milwaukee)
      (pittsburg . Pittsburgh) 
      (cincinnatti . cincinnati)
      (nuffalo . buffalo)
      (Inianapolis . indianapolis)
      (volumbus . columbus)
      (chivago . chicago)
      (NewYok . new_york)
      (NewYork . new_york)
      (Milaukee . milwaukee)
      (Indianopolis . indianapolis)
      (charlston . charleston)
      (charrleston . charleston)
      (Ney . new)
      (columbu . columbus)
      (Washingto . washington)
      (clevland . cleveland)
      (scanton . scranton)
      (milwakee . milwaukee)
      (oledo . toledo)
      (colombus . columbus)
      (chleston . charleston)
      (albamny . albany)
      (Pitsburgh . pittsburgh)
      (Milwalkee . milwaukee)
      (raleight . raleigh)
      (ilwaukee . milwaukee)
      (TRONTO . toronto)
      (BUFALLO . buffalo)
      (INDIANAOPOLIS . indianapolis)
      (PHILEDELPHIA . philadelphia)
      (SHARLESTON . charleston)
      ;; this next set are the non-cities from the evaluation
      (I^M . nil)
      (SOND . nil)
      (TRHOUGH . nil)
      (THATS . nil)
      (THENTHEN . nil)
      (TALKING . nil)
      (OTHER . nil)
      (TALK . nil)
      (SKIP . nil)
      (ELSE . nil)
      (COMMAND . nil)
      (SNED . nil)
      (UNDERSTAND . nil)
      (IM . nil)
      (RATHER . nil)
      (VI . nil)
      (REDO . nil)
      (PHILADELPHIAINSTEAD . nil)
      (VAI . nil)
      (REMOVE . nil)
      (CANADA . nil)
      (CA . nil)
      (FORM . nil)
      (ROUTEE . nil)
      (GOTCHA . nil)
      (KILL . nil)
      (UMM . nil)
      (WWHY . nil)
      (DELETE . nil)
      (AQUA . nil)
      (ERASE . nil)
      (FROGET . nil)
      (NTO . nil)
      (DOME . nil)
      (COURSE . nil)
      (FINALLYT . nil)
      (ROUT . nil)
      (THA . nil)
      ;; here are a few more potentials
      (cincinatti . cincinnati)
      (cinncinnatti . cincinnati)
      (cinncinnati . cincinnati)))

(defun test-speller()
  (mapcar #'(lambda (pair)
	      (let ((corrected (speeling-corrector (car pair) *threshold*)))
		(cond ((eq corrected (cdr pair))
		       (format t "~A correctly corrected to ~A~%"
			       (car pair) corrected))
		      ((eq corrected (car pair))
		       (format t "~A: unchanged~%" (car pair)))
		      (t
		       (format t "~A mistakenly changed to ~A~%"
			       (car pair) corrected)))))
	  *typos*))
				  
;; for now, just deal with city names, since that's the most common blunder.
(let ((wordlist nil)
      (parser-pkg (find-package :parser)))

  (defun reset-wordlist (&optional words)
    (if words
        (setq wordlist words)
      (setq wordlist (mapcar #'(lambda (x)
                                 (intern (basic-name x) parser-pkg))
                             (kb-request :world-kb :all-cities)))))
  
  (defun speeling-corrector (token &optional (threshold 100))
    (if wordlist
      (let (( *insert-cost* (adjust-cost 'ins (length (string token))))
	    ( *delete-cost* (adjust-cost 'del (length (string token))))
	    ( *typo-cost* (adjust-cost 'typo (length (string token)))))
        (cond
         ((member token wordlist :test #'eq)
	  (values token 1000))
         (t
	  ;; find closest match, rank them.
	  (let* ((matchlist (find-match-to-wordlist token wordlist))
	         (smatch (find-max (cons 0 token) matchlist :key #'car)))
	    ;; if best match is poor, return original token.
	    (if (< (car smatch) threshold)
	      token
	      (values (cdr smatch)
		      (car smatch)))))))
      token))
  ) ;; end scope of WORDLIST
  
(defun find-match-to-wordlist (token wordlist)
  ;; rate how close token is to each word in wordlist. 1000 is perfect match.
  ;; return alist, key is rating, value is word in wordlist.
  (unless (null wordlist)
    (cons (rate-word token (car wordlist))
          (find-match-to-wordlist token (cdr wordlist))))
  )

(defun rate-word (token word)
  (cons (closeness token word)
        word))

(defun closeness (token word)
  (let ((score 1000)
        (tname (copy-seq (string token)))
        (wname (string word)))
    (loop
      (cond
       ((equalp tname wname)
        (return-from closeness score))
       ;; allow single mutation of token, at a cost.
       ((eql (length tname) (length wname))
        (setq score (string-closeness tname wname score))
        (return-from closeness score))
       ((< (length tname) (length wname))
        ;; add first char that differs
        (setq tname (addchar tname wname))
        (decf score *insert-cost*))
       (t
        (setq tname (subchar tname wname))
        (decf score *delete-cost*))))))

(defun subchar (mod target)
  (do ((mchar 0 (1+ mchar))
       (tchar 0 (1+ tchar)))
      ((or (eql tchar (length target))
           (not (eql (char mod mchar) (char target tchar))))
       (concatenate 'string
         (subseq mod 0 mchar)
         (subseq mod (1+ mchar))))))

(defun addchar (mod target)
  (do ((mchar 0 (1+ mchar))
       (tchar 0 (1+ tchar)))
      ((or (eql mchar (length mod))
           (not (eql (char mod mchar) (char target tchar))))
       (concatenate 'string
         (subseq mod 0 mchar)
         (string (char target tchar))
         (if (eql mchar (length mod))
             ""
           (subseq mod mchar))))))

(defparameter *DEBUG-SPEELING* nil)

(defun string-closeness (mod target score)
  ;; strings are same length, count changes or transpositions (differently).
  (do ((mchar 0 (1+ mchar))
       (tchar 0 (1+ tchar)))
      ((eql mchar (length mod)) score)
    (cond
     ;; correct this char?
     ((eql (char mod mchar) (char target tchar))) 
     ;; simple transpose?
     ((and (not (eql tchar (1- (length mod)))) ; not at end of string
       (eql (char mod mchar) (char target (1+ tchar)))
       (eql (char mod (1+ mchar)) (char target tchar))) 
      (if *debug-speeling* (format t "simple xpose~%"))
      (decf score *simple-transpose-cost*)
      (setf (char mod mchar) (char target tchar))
      (setf (char mod (1+ mchar)) (char target (1+ tchar))))
     ;; simple typo?
     ((or (eql tchar (1- (length mod)))
       (eql (char mod (1+ mchar)) (char target (1+ tchar))))
      (if *debug-speeling* (format t "simple typo~%"))
      (decf score *typo-cost*)
      (setf (char mod mchar) (char target tchar)))
     ;; rotation?
     ((check-rotate mod mchar target tchar)
      (let ((foo (check-rotate mod mchar target tchar)))
	(if *debug-speeling* (format t "rotate~%"))
	(decf score (* *rotation-cost* foo))
	(dotimes (i foo)
	  (setf (char mod (+ mchar i)) (char target (+ tchar i))))))
     ;; bad typo
     (t
      (decf score *bad-typo-cost*)
      (if *debug-speeling* (format t "bad typo~%"))
      (setf (char mod mchar) (char target tchar))))))
     
(defun check-rotate (mod mchar target tchar)
  (dotimes (i (- (length mod) mchar))
    (let ((foo (check-rotate-i mod mchar target tchar i)))
      (if foo (return-from check-rotate foo))
      foo)))


(defun check-rotate-i (mod mchar target tchar rot)
  "return rot if the n chars in rot starting in mod mchar posn are rotated
from target, tchar position."
  (cond
   ((and (zerop rot)
         (eql (char mod mchar) (char target tchar)))
    rot)
   ((and (eql rot 1)
         (eql (char mod (1+ mchar)) (char target tchar))
         (eql (char mod mchar) (char target (1+ tchar))))
    rot)
   ((and (eql (char mod (+ mchar rot)) (char target tchar))
         (equalp (subseq mod mchar (+ mchar rot))
                 (subseq target (1+ tchar) (+ 1 tchar rot))))
    rot)))

(defun find-max (token l &key (key #'identity))
  "find the maximum entry on l, using key if needed."
  (let* ((max-so-far (car l))
	(res (find-max-i token (cdr l) max-so-far key)))
    (if res
	res
      token)
    ))

(defun find-max-i (token l max-so-far key)
  (cond
   ((null l)
    (if (equal max-so-far token)
	nil
      max-so-far))
   ((> (funcall key (car l))
       (funcall key max-so-far))
    (find-max-i token (cdr l) (car l) key))
   ((equal (funcall key (car l))   ;; if we found 2 closest matches, resent the closed found to token
	   (funcall key max-so-far))
    (find-max-i max-so-far (cdr l) max-so-far key))
   (t
    (find-max-i token (cdr l) max-so-far key))))














