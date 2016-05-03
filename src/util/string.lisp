;;;; string.lisp - string manipulation functions (originally from weblearn)

(in-package "UTIL")

(defun alphanumeric-stringp (str)
  "@returns True if the string is all letters and numbers (no punctuation)"
  (let (ch)
    (dotimes (i (length str) t)
      (setq ch (char str i))
      (if (and (not (digit-char-p ch))
	       (not (alpha-char-p ch)))
	  (return nil)))))

(defun numeric-stringp (str)
  "@returns True if the string is all numbers"
  (dotimes (i (length str) t)
    (if (not (digit-char-p (char str i))) (return nil))))

(defun replace-hyphens (word)
  "@param word A string (or stringable atom)
   @returns The same string, but with spaces where hyphens were"
  (if (not (stringp word)) (setq word (string word)))
  (let ((pos (position #\- word)))
    (if pos (format nil "~A ~A" (subseq word 0 pos) (replace-hyphens (subseq word (1+ pos))))
      word)))

;; TODO?  7-12 -> "seven to twelve"
;;        35%  -> "thirty five percent"
(defun normalize-string (str)
  "@param str A string of characters: words and punctuation
   @returns The string in 'readable' format with no punctuation or spurious space"
  (setq str (remove-punctuation str))
  (let ((words (split-string (string-downcase str)))
	(full ""))
    (dolist (word words)
      (when (> (length word) 0)
	;; convert numbers to words
	(if (numeric-stringp word) (setq word (number-to-string (read-from-string word))))
	(setq full (format nil "~A ~A" full word)))
      )
    (crop-string full)))

(defun remove-punctuation (word)
  "@desc Removes all punctuation and extra whitespace from the word"
  (cond ((numberp word) (setq word (format nil "~A" word)))
	((not (stringp word)) (setq word (string word))))

  ;; replace all & symbols first
  (setq word (replace-& word))
  ;; now remove all punctuation
  (let ((new-word "") ch)
    (dotimes (i (length word))
      (setq ch (char word i))
      ;; if not a letter or number, put a whitespace down
      (cond ((and (not (alphanumericp ch)) (not (member ch '(#\&))))
	     (if (or (= (length new-word) 0)
		     (char-equal #\Space (char new-word (1- (length new-word)))))
		 nil ;; do nothing
	       (setq new-word (format nil "~A " new-word))))
	    (t
	     (if (= 0 (length new-word)) (setq new-word (format nil "~A" ch))
	       (setq new-word (format nil "~A~A" new-word ch))))))
    new-word))

(defun replace-& (str)
  "@desc Replace all & symbols with the word 'and' if it is surronded by whitespace,
         and just remove it if it is in the middle of words."
  (let ((pos (position #\& str)))
    (cond (pos
	   (if (> pos 0)
	       (if (< pos (length str))
		   (if (and (char= #\Space (char str (1- pos)))
			    (char= #\Space (char str (1+ pos))))
		       (format nil "~Aand~A" (subseq str 0 pos) (replace-& (subseq str (1+ pos))))
		     (format nil "~A~A" (subseq str 0 pos) (replace-& (subseq str pos))))
		 ;; just skip it if it ends the string
		 (subseq str 0 pos))
	     ;; just skip it if it leads the string
	     (replace-& (subseq str 1))))
	  (t str))))


(defun replace-space-with-hyphen (word)
  "@param word A string
   @returns The same string, but with hyphens where spaces were"
  (if (not (stringp word)) (setq word (string word)))
  (let ((pos (position #\Space word)))
    (if pos (format nil "~A-~A" (subseq word 0 pos) (replace-spaces (subseq word (1+ pos))))
      word)))

(defun replace-spaces (word)
  "@param word A string
   @returns The same string, but with hyphens where spaces were"
  (if (not (stringp word)) (setq word (string word)))
  (let ((pos (position #\Space word)))
    (if pos (format nil "~A-~A" (subseq word 0 pos) (replace-spaces (subseq word (1+ pos))))
      word)))

(defun crop-string (str)
  "Remove leading/trailing whitespace from a string.
   Also replace any hyphens - with a blank space."
  (string-trim " " (substitute #\Space #\- str)))

(defun split-string (str &key (on '(#\Space #\Newline)))
  "@desc Returns a list of strings which are the sequences in 'str' that
         are separated by the character 'on'"
  (if (atom on) (setq on (list on)))
  
  (dotimes (x (length str) (list str))
    (if (member (char str x) on :test #'equal)
	(return-from split-string
	  (cons (subseq str 0 x)
		(split-string (subseq str (1+ x) (length str)) :on on))))
    ))

(defun count-words (str)
  "@returns The number of words in the string - really the number of whitespaces
            Use this instead of split-string if you just need a count, it is faster"
  (let ((count 1))
    (dotimes (i (length str))
      (if (eql #\Space (char str i)) (setq count (1+ count))))
    count))

;; --------------------------------------------------
;; String to Number Conversion
;; --------------------------------------------------
(defun number-to-string (num)
  (let ((ll (number-to-strlist num)))
    (if (listp ll)
	(if (= 1 (length ll)) (car ll)
	  (let ((final (car ll)))
	    (dolist (num (cdr ll))
	      (setq final (format nil "~A ~A" final num)))
	    final))
      ll)))

(defun number-to-strlist (num)
  "Converts a number into its string form (up to 999,999)"
  (cond ((floatp num)
	 (list (format nil "~S" num))) ;; just return it for now
	((numberp num)
	 (cond ((>= num 1000)
		(append (number-to-strlist (floor (/ num 1000)))
			'("thousand")
			(if (< 0 (- num (* (floor (/ num 1000)) 1000)))
			    (number-to-strlist (- num (* (floor (/ num 1000)) 1000))))))
	       ((>= num 100)
		(append (number-to-strlist (floor (/ num 100)))
			'("hundred")
			(if (< 0 (- num (* (floor (/ num 100)) 100)))
			    (number-to-strlist (- num (* (floor (/ num 100)) 100))))))
	       ((> num 19)
		(let ((big (* (floor (/ num 10)) 10))
		      (small (mod num 10)))
		  (if (= 0 small)
		      (list (base-num-to-string big))
		      ;(base-num-to-string big)
		    (append (list (base-num-to-string big))
			    (list (base-num-to-string small)))))
		)
	       (t ;(base-num-to-string num))))
		(list (base-num-to-string num)))))
	(t num)))

(defun base-num-to-string (num)
  "Converts atomic numbers to strings (less than 20 and the ten's)"
  (second (assoc num '(
    (0 "zero") (1 "one") (2 "two") (3 "three") (4 "four")
    (5 "five") (6 "six") (7 "seven") (8 "eight") (9 "nine")
    (10 "ten") (11 "eleven") (12 "twelve") (13 "thirteen")
    (14 "fourteen") (15 "fifteen") (16 "sixteen") (17 "seventeen")
    (18 "eighteen") (19 "nineteen")
    (20 "twenty") (30 "thirty") (40 "forty") (50 "fifty")
    (60 "sixty") (70 "seventy") (80 "eighty")
    (90 "ninety"))))
  )

(defun unit-position (num &optional (count 1))
  "@returns The unit of the number: 9384 -> 4, 82 -> 2, 8 -> 1"
  (if (< num 10) count
    (unit-position (floor (/ num 10)) (1+ count))))
