;;;;
;;;; wordnet lisp interface
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 29 Apr 2003
;;;; Time-stamp: <Thu Jul  6 10:47:42 EDT 2006 swift>
;;;;
;;;; Modified by David McClosky, mcclosky@cs.rochester.edu
;;;;

;(defpackage :wordnet
;  (:use :common-lisp))
;
;(in-package :wordnet)
(in-package :wordfinder)
;;;
;;; Utility functions
;;;

; We might not want to use this if we wanted to cache the file-length,
; but it does not seem to hinder performance.  (Perhaps files remember their
; length?)
(defun eof? (file)
  "Returns true if the file is at the end"
  (= (file-position file) (file-length file)))

(defun print-hash (hash)
  "Given a hashtable, prints all entries in the form 'key = value'"
  (maphash #'print-hash-values hash))

(defun print-hash-values (k v)
  "Helper function for print-hash that actually prints the entries of a hashtable."
  (format t "~A = ~A~%" k v))

(defun string-ends-with (str suffix)
  "Returns whether str has the string suffix as a suffix."
  (let* ((search-result (search suffix str :from-end t)))
        (and search-result
             (= search-result
                (- (length str)
                   (length suffix))))))

; I couldn't find a general substring replacer or even a general sequence
; replacer, so I wrote this ad hoc fix.
(defun string-replace-suffix (new-suffix old-suffix str)
  "If str ends with old-suffix, returns a new string with the new-suffix instead of the old-suffix.  Otherwise, returns nil."
  (if (string-ends-with str old-suffix)
      ; chop off the old suffix
      (let ((chopped-str (subseq str 0 (- (length str) (length old-suffix)))))
           ; attach the new suffix to the stem
           (concatenate 'string chopped-str new-suffix))))

; This function is by Mike Travers
; Source: http://aracyc.stanford.edu/~jshrager/jeff/mbcs/anno1.html
(defun string-split (string &optional (delimiter #\space))
  "Split a string into a list of words by a delimeter"
  (let ((substrings '())
        (length (length string))
        (last 0))
    (flet ((add-substring (i)
             (push (make-array (- i last)
                               :element-type 'character
                               :displaced-to string
                               :displaced-index-offset last)
                   substrings)))
      (dotimes (i length)
        (when (eql (char string i) delimiter)
          (add-substring i)
          (setq last (1+ i))))
      (add-substring length)
      (nreverse substrings))))

(defun load-into-hash (hashtable pairs)
  "Store a list of pairs into the hash table.  The pairs should be in this form: (key value) and will be stored as such in the hash table."
  (dolist (pair pairs)
          (setf (gethash (car pair) hashtable) (cadr pair))))

; From http://www.cogsci.princeton.edu/~wn/man1.7.1/wninput.5WN.html
(defvar pointer-type-descs (make-hash-table :test #'equal))
(load-into-hash pointer-type-descs 
                '(("!"  antonym)
                  ("@"  hypernym)
                  ("~"  hyponym)
                  ("#m" member-holonym)
                  ("#s" substance-holonym)
                  ("#p" part-holonym)
                  ("%m" member-meronym)
                  ("%s" substance-meronym)
                  ("%p" part-meronym)
                  ("="  attribute)
                  ("*"  entailment)
                  (">"  cause)
                  ("^"  also-see)
                  ("$"  verb-group)
                  ("&"  similar-to)
                  ("<"  particle-of-verb)))
                  ; we leave out: '\' which can be a pertainym (for nouns)
                  ; or derived-from-adj (for adverbs).  this is handled
                  ; later in pointer-description.

; This is a mapping from the part of speech symbols given in synsets to their
; longer names used in filenames and extensions.
; See pos-sym-to-string
(defvar pos-char-abbrevs (make-hash-table :test #'equal))
(load-into-hash pos-char-abbrevs 
                '(("a" "adj")
                  ("s" "adj") ; s = adjective satellite, which are found in the
                              ;     same places as adjectives, so we don't draw
                              ;     a distinction here
                  ("n" "noun")
                  ("r" "adv")
                  ("v" "verb")))

(defun pos-sym-to-string (pos-sym)
  "Given a part of speech symbol ('|a|, '|s|, '|n|, '|r|, '|v|), returns a string describing the part of speech.  The description can be used to construct filenames for the WordNet database."
  (gethash (string pos-sym) pos-char-abbrevs))

; Morphological Rules of Detachment
; This is used to help reimplement Morphy in lisp.
; See http://aracyc.stanford.edu/~jshrager/jeff/mbcs/anno1.html
(defvar morph-rules-of-detachment (make-hash-table :test #'equal))
(load-into-hash morph-rules-of-detachment
                '(("noun" (("s" "")
                           ("ses" "s")
                           ("xes" "x")
                           ("zes" "z")
                           ("ches" "ch")
                           ("shes" "sh")
                           ("men" "man")
                           ("ies" "y")))
                  ("verb" (("s" "")
                           ("ies" "y")
                           ("es" "e")
                           ("es" "")
                           ("ed" "e")
                           ("ed" "")
                           ("ing" "e")
                           ("ing" "")))
                  ("adj"  (("er" "")
                           ("est" "")
                           ("er" "e")
                           ("est" "e")))
                  ("adv"  ()))) ; we include this so lookup will always succeed

;;;
;;; Index filehandling
;;;

(defclass wordnet-index ()
  (entries))

(defclass wordnet-index-entry ()
  (lemma
   pos
   synset-cnt
   p-cnt
   ptr-symbols
   sense-cnt
   tagsense-cnt
   synset-offsets))

(defmethod print-object ((this wordnet-index-entry) stream)
  (with-slots (lemma pos) this
    (format stream "#<wn-index-entry ~A ~A>" lemma pos)))

(defmethod dump-object ((this wordnet-index-entry) &optional (stream t))
  (with-slots (lemma pos synset-cnt p-cnt ptr-symbols sense-cnt
	       tagsense-cnt synset-offsets) this
    (format stream "\"~A\" ~A~%  ptr-symbols[~S] = ~S~%  synset-offsets[~S] = ~S~%"
	    lemma pos p-cnt ptr-symbols synset-cnt synset-offsets)))

(defun read-index-entry (s)
  "Reads the next entry of a WordNet index from stream S and returns the
resulting WORDNET-INDEX-ENTRY."
  (let ((entry (make-instance 'wordnet-index-entry)))
    (with-slots (lemma pos synset-cnt p-cnt ptr-symbols sense-cnt
		 tagsense-cnt synset-offsets) entry
      (setq lemma (read-token s))
      (setq pos (read-token s))
      (setq synset-cnt (read-number s))
      (setq p-cnt (read-number s))
      (setq ptr-symbols (read-token-list s p-cnt))
      (setq sense-cnt (read-number s)) ;; this is actually redundant...
      (setq tagsense-cnt (read-number s))
      (setq synset-offsets (read-number-list s synset-cnt))
      (skip-to-newline s))
    entry))

(defun read-index (s)
  "Reads WordNet index from stream S and returns the resulting WORDNET-INDEX."
  (skip-comments s)
  (let ((index (make-instance 'wordnet-index)))
    (with-slots (entries) index
		(dolist (entry  (read-index-entry s))
		  (push entry entries))
		(setf entries (nreverse entries)))
    index))

(defvar token-buffer nil
  "Buffer used to store characters read in READ-TOKEN, since WordNet's idea
of a token is not the same as Common Lisp's. An alternative would be to
reprogram the Lisp reader via its readtable.")
(setq token-buffer (make-array 256 :element-type 'character
			       :adjustable t :fill-pointer 0))

(defun read-token (s)
  "Reads the next run of non-whitespace characters and returns them as a
symbol. Signals a condition if EOF is encountered."
  (setf (fill-pointer token-buffer) 0)
  (loop
      with ch
      while (and (setq ch (read-char s))
		 (not (char= ch #\space)))
      do (vector-push-extend ch token-buffer))
  (if (eql (length token-buffer) 0)
      nil
    (intern token-buffer)))

(defun read-token-list (s n)
  "Returns a list of N tokens read from S."
  (let ((items nil))
    (loop for i from 0 below n
	do (push (read-token s) items))
    (nreverse items)))

(defun read-string (s)
  "Reads the next run of non-whitespace characters and returns them as a
string. Signals a condition if EOF is encountered."
  (setf (fill-pointer token-buffer) 0)
  (loop
      with ch
      while (and (setq ch (read-char s))
		 (not (char= ch #\space)))
      do (vector-push-extend ch token-buffer))
  (copy-seq token-buffer))

(defun read-number (s)
  "Reads and returns the next number (decimal integer) from stream S.
We can just let the Lisp reader do its thing here. Signals a condition if
EOF is encountered."
  (read s))

(defun read-number-list (s n)
  "Returns a list of N numbers read from S."
  (let ((items nil))
    (loop for i from 0 below n
	do (push (read-number s) items))
    (nreverse items)))

(defun read-hex-number (s)
  "Reads and returns the next hexadecimal number from stream S.
We can just let the Lisp reader do its thing here. Signals a condition if
EOF is encountered."
  (let ((*read-base* 16))
    (read s)))

(defun skip-comments (s)
  (loop while (skip-comment s)))

(defun skip-comment (s)
  "Skips comment at the start of the WordNet file in stream S.
Comments start with two spaces and a line number, although we just check
for the first space and gobble the rest of the line. Returns T if a comment
was skipped. Does not signal a condition if EOF is encountered (but just
returns NIL)."
  (let ((ch (read-char s nil nil)))
    (cond ((not (char= ch #\space))
	   (unread-char ch s)
	   nil)
	  (t
	   (loop
	       while (and (setq ch (read-char s nil nil))
			  (not (char= ch #\newline))))
	   t))))

(defun skip-to-newline (s)
  (loop
      while (not (char= (read-char s) #\newline))))

;;;
;;; Data file handling
;;;

(defclass wordnet-synset ()
  (lex-filenum
   ss-type
   w-cnt
   words
   lex-id
   p-cnt
   ptrs
   frames
   glosses
   offset))

(defmethod print-object ((this wordnet-synset) stream)
  (with-slots (ss-type words offset) this
    (format stream "#<wn-ss ~A ~S ~A>" ss-type
	    (mapcar #'(lambda (w)
			(slot-value w 'word)) words)
        offset)))

(defmethod dump-object ((this wordnet-synset) &optional (stream t))
  (with-slots (ss-type words glosses) this
    (format stream "~A: ~S~%" ss-type (mapcar #'(lambda (w)
						  (slot-value w 'word)) words))
    (loop for s in glosses
	do (format stream "  \"~A\"~%" s))))

(defmethod synsets-equal-p ((ss1 wordnet-synset) (ss2 wordnet-synset))
  "Do the given wordnet-synset structures represent the same synset?"
  (and (= (slot-value ss1 'lex-filenum)
          (slot-value ss2 'lex-filenum))
       (= (slot-value ss1 'offset)
          (slot-value ss2 'offset))))

(defmethod get-pos-string ((this wordnet-synset))
  "Returns the part of speech string for a synset."
  (pos-sym-to-string (slot-value this 'ss-type)))

(defmethod get-offset ((this wordnet-synset))
  "Returns the offset for a synset."
  (slot-value this 'offset))

(defmethod get-ss-type ((this wordnet-synset))
  "Returns the offset for a synset."
  (slot-value this 'ss-type))

(defmethod get-lex-filenum ((this wordnet-synset))
  "Returns the offset for a synset."
  (slot-value this 'lex-filenum))

(defmethod get-synonyms ((this wordnet-synset))
  "Returns all words in the synset.  They will have the underscores converted to spaces."
  (mapcar #'(lambda (w)
                    (substitute #\Space #\_ (slot-value w 'word)))
          (slot-value this 'words)))

(defmethod wn-lemmas ((this wordnet-synset))
  "Returns all words in the synset.  Preserves underscores."
  (mapcar #'(lambda (w)
	      (slot-value w 'word))
           (slot-value this 'words)))

(defmethod sense-key-for-word-and-synset (word (this wordnet-synset))
  "Returns the sense-key for the specific word in the synset."
  (remove-if #'null (mapcar #'(lambda (w)
				(when (lemmas-equal-p (slot-value w 'word) word)
				  (get-sense-key this (slot-value w 'word))))
			    (slot-value this 'words)))
  )

(defmethod sense-keys-for-synset ((this wordnet-synset))
  "Returns all sense-keys for the synset."
  (remove-duplicates (mapcar #'(lambda (w)
	      (get-sense-key this (slot-value w 'word)))
           (slot-value this 'words)) :test #'equal)
  )

(defmethod get-verb-frame-numbers ((this wordnet-synset))
  "If the synset is a verb, return a list of verb frame numbers.  Otherwise, return nil."
  (with-slots (ss-type frames) this
    (if (equal (string ss-type) "v") ; only verbs have frames
        frames)))

; A word within a synset.
(defclass wordnet-synset-word ()
  (word
   lex-id))

(defmethod print-object ((this wordnet-synset-word) stream)
  (with-slots (word lex-id) this
    (format stream "#<wn-ss-word ~S ~D>" word lex-id)))

(defmethod dump-object ((this wordnet-synset-word) &optional (stream t))
  (with-slots (word lex-id) this
    (if (eql lex-id 0)
	(format stream "~A" word)
      (format stream "~A[~D]" word lex-id))))

; Describes the relation between one synset and another.
(defclass wordnet-synset-ptr ()
  (pointer-symbol
   synset-offset
   pos
   source-target))

(defmethod print-object ((this wordnet-synset-ptr) stream)
  (with-slots (pointer-symbol synset-offset pos source-target) this
    (format stream "#<wn-ss-ptr ~A ~D ~A ~0,4D>"
	    pointer-symbol synset-offset pos source-target)))

(defmethod pointer-description ((this wordnet-synset-ptr))
  "Returns a symbol describing the pointer type, as described in http://www.cogsci.princeton.edu/~wn/man1.7.1/wninput.5WN.html"
  (with-slots (pointer-symbol pos) this
    (if (eql pointer-symbol "\\")
        (if (equalp pos '|n|)      ; if it's a noun,
            'pertainym         ; \ means it's a pertainym
            'derived-from-adj) ; otherwise, it's derived from an adjective
        (gethash pointer-symbol pointer-type-descs))))

(defun read-synsets (s entry)
  "Returns a list of synset entries for each sense of ENTRY in S."
  (with-slots (synset-cnt) entry
    (loop
	for i from 0 below synset-cnt
	collect (read-synset s entry i))))

(defun read-synset (s entry i)
  "Reads from S and returns the data for the I'th synset in the index
entry ENTRY."
  (with-slots (synset-offsets) entry
    (let ((offset (nth i synset-offsets)))
      (file-position s offset)
      (read-synset-entry s))))

(defun read-synset-entry (s &key (synset-offset nil))
  "Assuming stream S is properly positioned, reads and returns the next
data entry.  Alternatively, you may specify an synset-offset."
  (if synset-offset
      (file-position s synset-offset))
  (let ((entry (make-instance 'wordnet-synset)))
    (with-slots (lex-filenum ss-type w-cnt words p-cnt ptrs
		 frames glosses offset) entry
      (setq offset (read-number s))
      (setq lex-filenum (read-number s))
      (setq ss-type (read-token s))
      (setq w-cnt (read-hex-number s))
      (setq words (read-word-list s w-cnt))
      (setq p-cnt (read-number s))
      (setq ptrs (read-ptr-list s p-cnt))
      (when (equal (string ss-type) "v") ;; only in data.verb
            (setq frames (read-frames s)))
      (setq glosses (read-glosses s))
      (skip-to-newline s)
      )
    entry))

(defun read-word-list (s n)
  "Returns a list of N word/lex-id pairs read from S."
  (let ((items nil))
    (loop for i from 0 below n
	do (push (read-word s) items))
    (nreverse items)))

(defun read-word (s)
  "Reads and returns the next word/lex-id pair from S."
  (let ((entry (make-instance 'wordnet-synset-word)))
    (with-slots (word lex-id) entry
      (setq word (read-string s))
      (setq lex-id (read-hex-number s)))
    entry))

(defun read-ptr-list (s n)
  "Returns a list of N date ptrs read from S."
  (let ((items nil))
    (loop for i from 0 below n
	do (push (read-ptr s) items))
    (nreverse items)))

(defun read-ptr (s)
  "Reads and returns the next data ptr from S."
  (let ((entry (make-instance 'wordnet-synset-ptr)))
    (with-slots (pointer-symbol synset-offset pos source-target) entry
      (setq pointer-symbol (read-token s))
      (setq synset-offset (read-number s))
      (setq pos (read-token s))
      (setq source-target (read-hex-number s))
      )
    entry))

; Example frame listing:
; 04 + 01 00 + 02 00 + 04 00 + 22 00
; cnt  f_num (decimal)
;         w_num (hex)
; w_num of 00 means "applies to all words"
; For that frame listing, we should return the following:
; ((1 0) (2 0) (4 0) (22 0))
(defun read-frames (s)
  "Read the frames for verbs, if there are any."
  (let ((cnt (read-number s))
        (frames '()))
       (dotimes (x cnt x)
         (read-token s) ; throw out the +
         (setf frames (append frames (list (list (read-number s) 
                                                 (read-hex-number s))))))
       frames))

(defun read-glosses (s)
  (let ((items))
    (loop
	while (skip-vertical-bar s)
	do (push (read-to-vertical-bar s) items))
    (nreverse items)))
    
(defun skip-vertical-bar (s)
  "Skips over the next vertical bar separating glosses (with whitespace also
skipped). Returns T if a vertical bar was found (and skipped) before a
newline."
  (let ((ch))
    ;; Skip to bar or newline
    (loop
	while (and (setq ch (read-char s nil nil))
		   (not (char= ch #\newline))
		   (not (char= ch #\|))))
    ;; If newline return nil
    (if (char= ch #\newline)
	;; And leave newline for later read
	(unread-char ch s)
      ;; Otherwise return T
      t)))

(defun read-to-vertical-bar (s)
  "Reads the string up to the next vertical bar or newline. Trims leading
and trailing whitespace. Leaves vertical bar or newline on the input.
Signals a condition if EOF is encountered."
  (setf (fill-pointer token-buffer) 0)
  ;; Skip whitespace
  (loop with ch
      while (char= (setq ch (read-char s)) #\space)
      finally (unread-char ch s))
  ;; Gather string
  (loop with ch
      while (and (setq ch (read-char s))
		 (not (char= ch #\newline))
		 (not (char= ch #\|)))
      do (vector-push-extend ch token-buffer)
      finally (unread-char ch s))
  ;; Trim trailing whitespace
  (vector-pop token-buffer)
  ;; Return (copy of) string
  (copy-seq token-buffer))

; The WordNet manager keeps track of WordNet indices, keeps track of
; WordNet filenames, and supplies any functions that require use of the
; WordNet indices (which is all except for those that work directly on
; synsets or pointers)  You will want to specify a :basepath (the path to
; the dict directory, including the dict directory) to the constructor,
; and possibly a list of parts of speech (:parts-of-speech) if you don't
; want all four parts of speech (noun, verb, adj, adv).  :indices and
; :exceptions should not be specified.
;; gf: Made default basepath be useless to avoid hiding pathname here
(defclass wordnet-manager ()
  ((basepath :initform "" :initarg :basepath)
   (parts-of-speech :initform '("noun" "verb" "adj" "adv"))
   (indices :initform (make-hash-table :test #'equal))
   (exceptions :initform (make-hash-table :test #'equal))
   (synset-cache :initform (make-hash-table :test #'equal))))

(defmethod load-indices ((this wordnet-manager))
  "Load indices for all parts of speech known."
  (with-slots (parts-of-speech) this
    (dolist (pos parts-of-speech)
      (load-index this pos))))

(defmethod load-exception-files ((this wordnet-manager))
  "Load exception lists for all parts of speech known."
  (with-slots (parts-of-speech exceptions) this
    (dolist (pos parts-of-speech)
      (setf (gethash pos exceptions) 
            (exception-file-to-hash-table (exception-file-path this pos))))))

(defmethod get-exceptions ((this wordnet-manager) (word string))
  "Get a list of (pos list-of-base-forms) pairs for each entry in the exception list with the specified word as the inflected form."
  (with-slots (parts-of-speech exceptions) this
    (let ((results ()))
         (dolist (pos parts-of-speech)
            (let* ((pos-exceptions (gethash pos exceptions))
                   (exception-entry (gethash word pos-exceptions)))
                  (if exception-entry
                      (push (list pos exception-entry) results))))
         results)))

(defmethod get-inflected-forms ((this wordnet-manager) (word string))
  "Get a list of (pos list-of-base-forms) pairs using the morphological rules of detachment.  See morph-rules-of-detachment for these rules."
  (with-slots (parts-of-speech exceptions) this
    (let ((results ()))
         (dolist (pos parts-of-speech)
                 ; look for a base form for each part of speech
                 (let ((new-forms (find-base-form word pos)))
                      (if new-forms
                          (setf results (append results 
                                                (list (list pos new-forms)))))))
	 (print-debug "~%inflected forms are ~S~%" results)
         results)))

(defmethod run-morphy ((this wordnet-manager) (word string))
  "This is a lisp implementation of morphy, the morphological engine in WordNet.  Returns all inflected forms with their part of speech.  First, we lookup the word in all exception lists, then we use the morphological rules of detachment to find possible inflections.  It returns a list of (pos list-of-new-forms) pairs.  I can't guarantee that the results of this will be the same as the real morphy, but they should be close (hopefully only a permutations)."
  (let ((irreg (get-exceptions this word))
        (reg (get-inflected-forms this word)))
    (append irreg
            ;; remove the regular forms for POSs that we already have irregular
	    ;; forms for, so that we end up with a true assoc list with no
	    ;; duplicate keys
	    (set-difference reg irreg :key #'car))))
  
(defmethod get-parts-of-speech ((this wordnet-manager))
  "Returns a list of parts of all speech that the WordNet manager is aware of."
  (slot-value this 'parts-of-speech))

(defvar core-wordnet-sense-keys (make-hash-table :test #'equalp))

(defmethod load-core-senses ((this wordnet-manager))
  "read the sense-key word pairs from the core wordnet senses text file into a hash table"
  (if (probe-file (core-senses-file-path this))
    (with-open-file (s (core-senses-file-path this))
		    (let ((line))
		      (loop while (setq line (read-line s nil nil)) do
			    (let ((sense-key  (subseq line (+ 1 (position #\[ line)) (position #\] line)))
				  (word  (subseq line (+ 1 (position #\[ line :from-end t)) (position #\] line :from-end t))))
			      (setf (gethash sense-key core-wordnet-sense-keys) word)
			    ))))
    (print-debug "**Wordfinder warning: core wordnet sense file not found**")
    ))

 ;; e.g., beautiful%3:00:00::
(defun is-core-wordnet-sense (sense-key)
  (or (gethash sense-key core-wordnet-sense-keys)
      (gethash (concatenate 'string sense-key "::")  core-wordnet-sense-keys)) ;; try adding final ::, not always present on sense-keys -- but all core senses have them
  )

(defmethod load-index ((this wordnet-manager) pos)
  "Load index for a specific part of speech.  Will clear the internal copy of the index first."
  (with-slots (indices) this
    (let ((pos-index (make-hash-table :test #'equal))) ; make new hash table
         (setf (gethash pos indices) pos-index) ; store it in the master hash
         (with-open-file (index-file (index-file-path this pos) 
                                     :direction :input)
           (skip-comments index-file)
           (loop
            with entry
            while (and (not (eof? index-file)) ; make sure we can still read
                       (setq entry (read-index-entry index-file)))
            do 
               (setf (gethash (string (slot-value entry 'lemma)) pos-index) 
                     entry))))))

(defmethod get-index ((this wordnet-manager) pos)
  "Get a hash table with the index for a specific part of speech.  This function is for internal and debugging purposes."
  (gethash pos (slot-value this 'indices)))

(defmethod get-index-entry ((this wordnet-manager) (word string) pos)
  "Get the wordnet-index-entry for a word (string) given a part of speech"
  (let ((index (get-index this pos)))
       (if index
           (gethash (string-downcase word) index)
           (error "You must load indices before accessing them."))))

(defmethod get-index-entry ((this wordnet-manager) word pos)
  "Get the wordnet-index-entry for a word (symbol) given a part of speech"
  (get-index-entry this (string word) pos))

(defun remove-stoplisted-synsets (word synsets)
  "Given a word and a list of synsets containing senses of that word, remove
   the synsets for which the sense is on the stoplist."
  (remove-if (lambda (ss)
               (let ((sks (sense-key-for-word-and-synset word ss)))
                 (when sks
		   (stoplist-p (car sks)))))
	     synsets))

(defmethod get-all-synsets ((this wordnet-manager) word
			    &key (use-morphy nil) (use-stoplist nil))
  "This method returns synsets that match a word for all parts of speech.  It can optionally use morphy to try to find a base form if the lookup fails. It can also optionally check senses against the stoplist."
  (let ((synsets
	  (apply #'append
		 (mapcar (lambda (pos)
			   (get-synsets this word pos))
			 (get-parts-of-speech this)))))
    (when use-stoplist
      (setf synsets (remove-stoplisted-synsets word synsets)))
       ; if they want to use morphy and we didn't get any synsets, we run
       ; morphy and feed the results to get-synsets-morpho, a function
       ; intended to be used in exactly this way
    ;; if use-morphy is t, use it even if we have synsets for the lemma -- e.g. to retrive both N and V for w::drafting (but if use-stoplist is t, retrieve only V "draft", because N "drafting" is on the stoplist)
    (when use-morphy ;; (and use-morphy (not synsets))
      (loop with new-forms = (run-morphy this word)
	    initially
	      (print-debug "morphy returns these forms ~S~%" new-forms)
	    for form in new-forms
	    append (get-synsets-morpho this form :use-stoplist use-stoplist)
	      into new-synsets
	    finally (setf synsets (append synsets new-synsets))
	    ))
    synsets))

(defmethod get-synsets ((this wordnet-manager) (entry wordnet-index-entry) pos)
  "Gets all synsets for a wordnet-index-entry and a specific part of speech"
  (with-open-file (data-file (data-file-path this pos) :direction :input)
                  (read-synsets data-file entry)))

(defmethod get-synsets ((this wordnet-manager) word pos)
  "Gets all synsets for a word (string) and a specific part of speech"
  (let ((index-entry (get-index-entry this word pos)))
       (if index-entry
           (get-synsets this index-entry pos))))

(defmethod get-synsets-morpho ((this wordnet-manager) 
                               (exception-list-form list)
			       &key (use-stoplist nil))
  "This gets all synsets for words mentioned in an exception list form, which has the form (part-of-speech list-of-inflected-forms).  For example: (\"noun\" (\"bus\")).  This is used by get-all-synsets when :use-morphy is activated."
  (let ((pos (first exception-list-form))
        (words (second exception-list-form)))
       (apply #'append 
              (mapcar (lambda (word)
	                (let ((synsets (get-synsets this word pos)))
			  (when use-stoplist
			    (setf synsets
				  (remove-stoplisted-synsets word synsets)))
			  synsets))
                      words))))

(defun lemmas-equal-p (l1 l2)
  "Are the two lemma strings equal, ignoring case, and ignoring the part in parentheses if only one of them has them?
  
  Some adjectives have parens after them, e.g. 'former(a)', (see 'Special Adjective Syntax' in the wninput(5wn) manpage), but that part often gets removed, so if only one of the two lemmas we're comparing has parens, we should just compare them without that part.
  "
  (let ((l1-paren-pos (position #\( l1))
        (l2-paren-pos (position #\( l2))
	)
    #\) #\) ; balance parens for not-smart-enough text editors
    (cond
      ((or (and l1-paren-pos l2-paren-pos)
           (not (or l1-paren-pos l2-paren-pos)))
        ;; both or neither have parens, just compare directly
	(string-equal l1 l2))
      (l1-paren-pos ; only
        (string-equal (subseq l1 0 l1-paren-pos) l2))
      (l2-paren-pos ; only
        (string-equal l1 (subseq l2 0 l2-paren-pos)))
      )))

(defun remove-parens-from-lemma (lemma)
  "Remove the stuff in parentheses at the end of a lemma."
  (let ((paren-pos (position #\( lemma)))
    #\) ; balance parens for not-smart-enough text editors
    (if paren-pos
      (subseq lemma 0 paren-pos)
      lemma)))

(defmethod get-synset-from-sense-key ((this wordnet-manager) sense-key)
  "Returns a wordnet-synset corresponding to sense-key.
   Sense-key is a string of the form lemma%ss-type:lex-filenum:lex-id,
   optionally followed by :: or :head-word:head-id, where ss-type is one digit,
   and lex-filenum, lex-id, and head-id are two digits. This is slightly more
   flexible than the actual definition, in that the final :: may be omitted. If
   :head-word:head-id is omitted from a satellite adjective sense key (ss-type
   5), the first matching sense is returned."
  (let* ((pct (position #\% sense-key))
         (lemma (subseq sense-key 0 pct))
         (ss-type (parse-integer (subseq sense-key (+ pct 1) (+ pct 2))))
	 (pos (nth (- (if (= ss-type 5) 3 ss-type) 1)
	           (slot-value this 'parts-of-speech)))
	 (lex-filenum (parse-integer (subseq sense-key (+ pct 3) (+ pct 5))))
	 (lex-id (parse-integer (subseq sense-key (+ pct 6) (+ pct 8))))
	 (has-head (and (= ss-type 5) (> (length sense-key) (+ pct 8))))
	 (synsets
	   ; match lemma and ss-type (AKA POS)
	   (get-synsets this (remove-parens-from-lemma lemma) pos))
	 )
    ;; prefer to match the specific ss-type of adjectives (head or satellite)
    (case ss-type
      (3
	(setf synsets (append (remove '|s| synsets :key #'get-ss-type)
			      (remove '|a| synsets :key #'get-ss-type))))
      (5
	(setf synsets (append (remove '|a| synsets :key #'get-ss-type)
			      (remove '|s| synsets :key #'get-ss-type))))
      )
    (find-if
      (lambda (synset)
	(and
	  ; match lex-filenum
	  (eql (slot-value synset 'lex-filenum) lex-filenum)
	  (let ((word ; find lemma among the words in this synset
		  (find-if
		    (lambda (w)
		      (with-slots (word) w
				  (lemmas-equal-p lemma word)))
		    (slot-value synset 'words)
		    )))
	      (and word
		   ; match lex-id
		   (eql (slot-value word 'lex-id) lex-id)
		   ))
	  (or (not (= ss-type 5))
	      (not has-head) ; if the head is missing, just take the first matching sense
	      ; match head, by matching whole sense key
	      (string= sense-key (get-sense-key synset lemma))
	      )
	  ))
      synsets
      )))

(defmethod get-synset ((this wordnet-manager) pos offset)
  "Given a part of speech and a specific offset, retrieves a synset from the data file."
  (let* ((cache (slot-value this 'synset-cache))
         (key (list pos offset))
         (old (gethash key cache)))
    (if old
      old
      (setf (gethash key cache)
	(with-open-file (wn-file (data-file-path this pos) :direction :input)
	  (read-synset-entry wn-file :synset-offset offset)))
      )))


(defmethod get-pointers ((this wordnet-manager) (synset wordnet-synset))
  "Returns a list of (wordnet-synset-ptr synset) pairs."
  (mapcar #'(lambda (ptr)
                  (with-slots (pos synset-offset) ptr
                              (list ptr (get-synset this 
                                                    (pos-sym-to-string pos) 
                                                    synset-offset))))
          (slot-value synset 'ptrs)))

; Example:
; (map-append '(3) '((a) (b c) (d)))
; ==> ((A 3) (B C 3) (D 3))
(defun map-append (itm lsts)
  "Appends a list (itm) to each list in a list of lists (lsts).  If lsts is nil, it returns (itm).  This is primarily used by get-hierarchy."
  (if lsts
      (mapcar (lambda (lst)
                      (append lst itm))
              lsts)
      (list itm)))

(defmethod get-pointers-by-relationship ((this wordnet-manager) 
                                         (synset wordnet-synset)
                                         pointer-char)
  "Given a synset and a relationship character (see pointer-type-descs) returns a list of (wordnet-synset-ptr synset) pairs that match that relationship."
  (mapcar (lambda (ptr)
            (with-slots (pos synset-offset) ptr
              (list ptr (get-synset this 
		                    (pos-sym-to-string pos) 
                                    synset-offset))))
	  (remove-if-not (lambda (ptr)
                           (equal (string (slot-value ptr 'pointer-symbol))
                                  pointer-char))
			 (slot-value synset 'ptrs))))

(defmethod get-parents ((this wordnet-manager) (synset wordnet-synset))
  "Returns a list of the parents (hypernyms) of a synset."
  (mapcar #'cadr (get-pointers-by-relationship this synset "@")))

(defmethod get-head-adjective ((this wordnet-manager) (synset wordnet-synset))
  "Returns the head adjective synset of a satellite adjective synset."
  (when (eq '|s| (get-ss-type synset))
    (cadar (get-pointers-by-relationship this synset "&"))))

;; the old get-hierarchy -- doesn't check for infinite loop in hierarchy search
;(defmethod get-hierarchy-old ((this wordnet-manager) (synset wordnet-synset))
;  "Returns all list of successive parents to the words in one synset."
;  (print-debug "getting hierarchy for ~S in ~S~%" this synset)
;  (let ((paths '())
;        (parents (get-parents this synset)))
;       (if parents         
;          (dolist (parent (get-parents this synset))
;                   (setf paths (append paths 
;                                       (map-append (list synset)
;                                                   (get-hierarchy this 
;                                                                  parent)))))
;           (setf paths (list (list synset))))
;       paths))

(defvar *parent-offset-list* nil) ;; keep a list of the parents to check if there's a loop in the WN definition, as there is in "limit"  with offsets  2422663 and  2423762

#| another old version; this one doesn't handle multiple inheritance correctly
   (and it annoyingly needs *parent-offset-list* cleared before calling it)
(defmethod get-hierarchy ((this wordnet-manager) (synset wordnet-synset))
  "Returns all list of successive parents to the words in one synset."
;  (print-debug "getting hierarchy for ~S in ~S~%" this synset)
  (let ((paths '())
        (parents (get-parents this synset)))
        (if parents
 	   (loop for parent in (get-parents this synset)
		 ;; check that we haven't already seen this parent before
		 while (not (member (get-offset parent) *parent-offset-list*))
		 do
		 (pushnew (get-offset parent) *parent-offset-list*)
;		 (print-debug "parent is ~S parent-list is ~S~%" parent *parent-offset-list*)
		 (setf paths (append paths 
				     (map-append (list synset)
						 (get-hierarchy this parent)))))
	 (setf paths (list (list synset))))
       paths))
|#

(defmethod get-hierarchy ((this wordnet-manager) (synset wordnet-synset))
  "Returns all lists of successive parents to the words in one synset."
  (loop with complete-paths = nil
        with paths = (list (list synset))
	while paths do
	(let* ((path (pop paths))
	       (child (car path))
	       (parents (get-parents this child)))
	  (if parents
	    (dolist (p parents)
	      (if (member p path)
	        (push path complete-paths) ; stop when a cycle is encountered
		(push (cons p path) paths)))
	    (push path complete-paths)
	    ))
	finally (return complete-paths)))

(defmethod index-file-path ((this wordnet-manager) pos)
  "Return the path of the index file for a given part of speech"
  (with-slots (basepath parts-of-speech) this
   (concatenate 'string basepath "index." pos)))

(defmethod data-file-path ((this wordnet-manager) pos)
  "Return the path of the data file for a given part of speech"
  (with-slots (basepath parts-of-speech) this
   (concatenate 'string basepath "data." pos)))

(defmethod exception-file-path ((this wordnet-manager) pos)
  "Return the exception file for a given part of speech (this is for morphological processing"
  (with-slots (basepath parts-of-speech) this
   (concatenate 'string basepath pos ".exc")))

(defmethod core-senses-file-path ((this wordnet-manager))
  "Return the path of the wordnet core senses file"
  (with-slots (basepath) this
   (concatenate 'string basepath "standoff-files/core-wordnet.txt")))

;;;
;;; Morphological Handling
;;;

(defun exception-file-to-hash-table (filename)
  "Loads a WordNet exception file (given as strings).  It returns a new hash table with the exceptions loaded."
  (let ((new-hash (make-hash-table :test #'equal)))
       (with-open-file (excfile filename :direction :input)
                       (do ((line (read-line excfile nil 'eof)
                                  (read-line excfile nil 'eof)))
                           ((eql line 'eof))
                           (let* ((line-pieces (reverse (string-split line)))
                                  (base (car line-pieces))
                                  (inflected (cdr line-pieces)))
                                 (insert-exception-entry new-hash base 
                                                         inflected))))
       new-hash))

(defun insert-exception-entry (hash base inflections)
  "Given an exception hash table, a base form of a word, and a list of inflections, we add base as a possible base form for each inflection."
  (dolist (inflection inflections)
          ; we add base to the end of that list
          (setf (gethash inflection hash)
                (append (gethash inflection hash '()) (list base)))))

(defvar *verb-inflections* '("s" "ies" "es" "ed" "ing"))
(defvar *plural-n-suffixes* '("s" "ses" "xes" "zes" "ches" "shes" "men" "ies"))
(defvar *comparative-inflections '("er" "est"))
(defvar *this-noun-inflection* nil)
(defvar *this-verb-inflection* nil)
(defvar *this-adj-inflection* nil)

(defun initialize-word-inflections ()
  (setf *this-noun-inflection* nil)
  (setf *this-verb-inflection* nil)
  (setf *this-adj-inflection* nil)
  )

(defun find-base-form (word pos)
  "Given a word and a part of speech, uses the morphological rules of detachment (as specified in morph-rules-of-detachment) to come up with new base forms.  Returns a list of possible (not necessarily correct) base forms."
  (let ((suffixes (gethash pos morph-rules-of-detachment))
        (results ()))
        (dolist (suffix suffixes)
               ; TODO: we shouldn't be using string-ends-with and 
               ; string-replace-suffix since the latter does the former anyway
               ; a let will let us optimize
	  (when (string-ends-with word (first suffix))
	    (setf results (cons (string-replace-suffix (second suffix)
						       (first suffix)
						       word)				
				results))
	    (case pos
	      ("noun" (pushnew (first suffix) *this-noun-inflection*))
	      ("verb" (pushnew (first suffix) *this-verb-inflection*))
	      ("adj" (pushnew (first suffix) *this-adj-inflection*))
	      (otherwise nil))
	    ))
	results))
