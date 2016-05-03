(in-package :wordfinder)

; XXX should be moved to utility file
(defun load-into-hash (hashtable pairs)
  "Store a list of pairs into the hash table.  The pairs should be in this form: (key value) and will be stored as such in the hash table."
  (dolist (pair pairs)
          (setf (gethash (car pair) hashtable) (cadr pair))))

;;;
;;; Data and hash table filling
;;;

; This is a special value used in COMLEX.  For our purposes, we can just
; consider it to be false.
(defvar *NONE* nil)

; This holds all COMLEX entries, keyed by orthography strings.  The value will 
; be a list of COMLEX entries (which are lists themselves).  If an entry
; has multiple spellings, it will have keys for each one.  For example:
; ((VERB :ORTH "ad-lib" :PAST "ad-libbed" :PASTPART "ad-libbed" 
;        :PRESPART "ad-libbing" :SUBC ((INTRANS) (NP)) 
;        :FEATURES ((VVERYVING :PRESPART T))))
; You can retrieve that key by requesting "ad-lib", "ad-libbed", or "ad-libbing"
(defvar comlex-hash-table (make-hash-table :test #'equal))

; These tell us how to map COMLEX features to TRIPS features and logical forms.
; Soon, WordNet will have a similar table, and we'll send the LF entries into
; the LF part of the entries.
(defvar comlex-features-to-trips (make-hash-table :test #'equal))
(load-into-hash comlex-features-to-trips
                '(("COUNTABLE" (w::MASS w::count))
                  ("NHUMAN" (LF ONT::PERSON))
                  ("GRADABLE" (w::GRADABILITY +))
                  ("VSAY" (LF ONT::STATEMENT))
                  ("NUNIT" (LF ONT::MEASURE-UNIT))
                  ("AGGREGATE" (LF ONT::GROUP))
                  ("NSCALE" (LF ONT::ORDERED-DOMAIN))
                  ("VMOTION" (LF ONT::MOTION))))

; Here are the major unmapped COMLEX features (features for 50 or more words):
; VVERYVING   (verb which either its present participle or its past participle 
;              can occur in adjectival positions with a left adjunct)
; ATTRIBUTIVE (adjective that can only be used as attribute)
; NCOLLECTIVE (a noun is an NCOLLECTIVE noun if it occurs as the non-plural 
;              subject of a collective verb when that verb has a null object.
;              "DUST gathered." or as a non-plural object of a collective verb
;              "the shelf will gather DUST")
; PREDICATIVE (adjective that can only be used as attribute)
; AIRN        (an adjective is in the class AINRN if it can occur as a 
;              single-word right adjunct of anoun)
; NTITLE      (title of a person)
; VCOLLECTIVE (requires its noun complement to be plural or conjoined)
; APREQ       (an adjective (or ving or ven form of the verb) is in APREQ if 
;              it occurs before aquantifier which is a left adjunct of N)
; See features-examples and features-annotated for a list of all features,
; their frequencies, and which words have them as features.
; These features are unmapped because there did not seem to be a
; corresponding TRIPS feature or logical form.  I have not yet handled
; how subcategorizations will be transferred, so some of these may be
; mapped later.

; This function is adapted from comlex-util.lisp's read-all-entries
(defun build-comlex-hashtable (filename)
  "Reads all COMLEX entries and places them in the comlex-hash-table."
  (let ((*package* (find-package :wordfinder)))
    (with-open-file (instream filename :direction :input)
      (do ((entry (read instream nil '*eof*)
		  (read instream nil '*eof*)))
	  ((equalp entry '*eof*))
	(add-comlex-entry entry)))))

(defun add-comlex-entry (entry)
  (let ((root (get-keyword-arg entry :orth)))
       ; Everything has an :orth
       (push entry (gethash root comlex-hash-table))

       ; Some things have other attributes, including :plural, :past, :pastpart,
       ; :prespart, :comparative, :superlative.  These can be lists or just 
       ; atoms.
       (dolist (kw '(:singular :plural :past :pastpart :prespart :comparative 
                               :superlative))
               (add-comlex-entry-forms entry kw root))))

(defun add-comlex-entry-forms (entry kw root)
  (let ((value (get-keyword-arg entry kw)))
       (if value
           ; We use listify, since forms can either be listed as an atom or in
           ; a list.  This lets us normalize our data and allows for simpler
           ; code.
           (let ((listified-value (listify value)))
                (dolist (form listified-value) ; add each form
                  (if (not (equal root form)) ; ...as long as it is not the same
                                              ; as the root.
                      (push entry (gethash form comlex-hash-table))))))))

;;;
;;; COMLEX entry conversion
;;;

(defun convert-comlex-entries (word)
  "This is the main function for COMLEX conversion.  Given a string, return a list of entries to send to the parser."
  (mapcar #'(lambda (entry)
	      (convert-comlex-entry entry word))
	  (get-comlex-entries word)))

(defun get-comlex-entries (word)
  "Returns all COMLEX entries associated with a given word."
  (gethash (string-downcase word) comlex-hash-table))

(defun convert-comlex-entry (entry original-word)
  "Convert a COMLEX entry into an entry to be handed back to the parser We convert part of speech, some orthography, some features, and subcategorizations (though we don't map subcats directly yet)"
  ; map COMLEX POS tags to TRIPS POS tags.  We only do open class words,
  ; as we have little/no chance of supplying a helpful definition of a function
  ; word
  (let ((mapped-pos (case (car entry) 
                          (VERB 'w::V)
                          (ADJECTIVE 'w::ADJ)
                          (NOUN 'w::N)
                          (ADVERB 'w::ADV))))
       (if mapped-pos ; if we didn't get a mapped-pos, we give up and return ()
           (let* ((form (find-form original-word entry))
                  (converted-features (convert-comlex-features 
                                       (get-keyword-arg entry :features)))
                  (comlex-subcats (get-keyword-arg entry :subc))
                  (mapped-features (first converted-features))
		  ; assume (w::mass w::mass) if (w::mass w::count) not present
		  ;; no -- need to assume both; e.g. "investment" is not marked as countable in comlex
;	          (amended-mapfeats (if (eql mapped-pos 'w::N)
;					(if (not (lxm::get-feature-values mapped-features 'w::mass))
;					    (push '(w::mass w::mass) mapped-features)
;					  mapped-features) mapped-features))
                  ; these are saved for debugging purposes, but are not sent
                  ; to the parser
                  (unmapped-features (second converted-features))
                  (build-args (list :constit-type mapped-pos 
                                    :features mapped-features 
                                    :comlex-subcats comlex-subcats
                                    :source :comlex)))
	       
                 ; if the entry is not the root form, we want to tell the 
                 ; parser that is is dealing with a derived form (and how 
                 ; it was derived)
                 (if (and form (not (eq form :root)))
                     (setf build-args 
                           (append (list :root (get-keyword-arg entry :orth)
                                         :form form) 
                                   build-args)))

                (apply #'build-entry 
                       (append (list original-word) build-args))))))

(defun in-comlex-keyword (word kw entry)
  "Returns whether 'word' is in the 'kw' keyword of 'entry' (a COMLEX entry)"
  (let ((kw-value (get-keyword-arg entry kw)))
       (if kw-value
           (member-or-equal word kw-value))))

(defun member-or-equal (itm atom-or-list &key (test #'equal))
  "If atom-or-list is an atom, check to see if they match (using :test). If it is a list, return whether it is a member."
  (if (listp atom-or-list)
      (member itm atom-or-list :test test)
      (apply test itm (list atom-or-list))))

; TODO: this function could have been done better
(defun find-form (word entry)
  "Returns where word can be found in entry (e.g. :root if it is in :orth, :past if it the :past form, etc."
  ; first, try to see if it is the main entry
  (cond ((in-comlex-keyword word :orth entry) :root)

         ; Verb forms
         ((in-comlex-keyword word :past entry) :past)
         ((in-comlex-keyword word :pastpast entry) :pastpast)
         ((in-comlex-keyword word :prespart entry) :ing)
        
         ; Noun forms
         ((in-comlex-keyword word :plural entry) :plural)
         ((in-comlex-keyword word :singular entry) :singular)

         ; Adjective forms
         ((in-comlex-keyword word :comparative entry) :er)
         ((in-comlex-keyword word :superlative entry) :est)))

; Based on Paul Graham's filter function
(defun convert-comlex-features (features)
  "Convert a list of COMLEX features to two lists.  The first list is a list of mapped COMLEX features for the features that mapped (i.e. these are COMLEX features).  The second list contains COMLEX features that did not map."
  (let ((mapped nil)
        (unmapped nil))
       (dolist (feature features)
         (let ((val (convert-comlex-feature feature)))
              (if val ; if we can convert it,
                  (push val mapped) ; put it in the mapped list
                  (push feature unmapped))))
       (list (nreverse mapped) (nreverse unmapped))))

(defun convert-comlex-feature (feature)
  "Return a matching TRIPS logical form or feature if available.  Otherwise, return nil."
  (gethash (string (car feature)) comlex-features-to-trips))

;;; 
;;; Debugging functions
;;;
;;; This functions is used to print out all possible subc or feature 
;;; values.  build-comlex-hashtable need not be run first.
;;; Example usage:
;;;   (print-entry-pieces :features) ; print all features
;;; or
;;;   (print-entry-pieces :subc) ; print all subcategorizations
;;;

(defun print-entry-pieces (key filename)
  "Prints out one specific key for all entries."
  ; loop over all entries
  (with-open-file (instream filename :direction :input)
                  (do ((entry (read instream nil '*eof*)
                              (read instream nil '*eof*)))
                      ((equalp entry '*eof*))
                      ; get the requested keyword
                      (let ((val (get-keyword-arg entry key)))
                      (if val ; print it if we have it
                          (dolist (itm (listify val))
                                  (format t "~A~%" itm)))))))

;;;
;;; Utility functions (should be moved to some common file)
;;;
(defun listify (itm)
  "If itm is not a list, make a list around it and return that."
  (if (listp itm)
      itm
      (list itm)))
