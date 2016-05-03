;;;;
;;;; scenario.lisp: Lexiconmanager scenario routines
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 10 May 1997
;;;; Time-stamp: <Mon Jan 18 12:57:14 EST 2010 ferguson>
;;;;

(in-package "LEXICONMANAGER")

;;;
;;; This is how a new scenario gets loaded
;;;

(defun load-new-scenario (scenario)
  "Loads lexicon definition for the given SCENARIO (name)
This is done by loading the defsys.lisp from the appropriate directory (if
it exists), then loading the :scenario-lexicon system (if one is defined)."
  (format *trace-output* "~&;; lxm: loading lexicon for scenario: ~A~%" scenario)
  (let ((defsys-path (trips::make-trips-pathname
		      (format nil "src;OntologyManager;Data;Domains;~A;defsys.lisp" scenario))))
    (cond
      ((probe-file defsys-path)
       (load defsys-path)
       (if (mk::get-system :scenario-lexicon)
	   (mk:load-system :scenario-lexicon)
	   (format *error-output* "~&lxm: warning: no scenario-lexicon system for scenario: ~A" scenario)))
      (t
       (format *error-output* "~&lxm: warning: no defsys for scenario: ~A~%" scenario))))
  (format *trace-output* "~&;; lxm: done loading lexicon for scenario: ~A~%" scenario))

;;
;; make-name-entry (object name class)
;; object  : an object id passed from the top -- we ignore for now
;;           but we may want to include it in name in the future
;; name    : either a string or list of strings, e.g. "Strong Memorial Hospital"
;; class   : kr-class (i.e. kr-type) to which the result belongs
;; returns : word sense definition for the name (the definition is added to the lexicon as a side-effect)
;;
(defun make-name-entry (object name class)
  "Adds a lexicon entry for OBJECT with NAME and CLASS, where NAME
may be a string."
  (cond
   ;; check if we have a list
   ((consp name)
    (mapcar #'(lambda (n) (make-name-entry object n class))
            name))
   ;; sanity check -- if it's not a list or a string, it's not a name!
   ((not (stringp name))
    (lexiconmanager-warn "name is not a string: ~S" name))
   (t
    ;; convert string to atom or list of atoms
    (setq name
      (let ((name-atoms (read-from-string (format nil "(~A)" name))))
	(cond ((eql (length name-atoms) 1)
	       (first name-atoms))
	      (t
	       name-atoms))))
    ;; extra processing for streets etc. to allow for abbreviations
    ;; e.g. allow "Monroe" for "Monroe Ave."
    (case class
	((ont::street ont::road ont::highway)
	 (add-street-to-lexicon name class))
	(t
	 ;; add name to lexicon
	 (add-name-to-lexicon (make-name-entry-with-sem name class))))
      )))


(defun add-name-to-lexicon (entry)
  "add a word-sense-definition to *lexicon-data* or appends this if it already exists"
  (let* ((word (word-sense-definition-word entry))
	 (rest (word-sense-definition-remaining-words entry))
	 (lex (lexicon-db-word-table *lexicon-data*))
	 (olddef (gethash word lex))
	 (rname (word-sense-definition-name entry))
	 )
    (setf (gethash word lex) 
      (cons
       (list :none rname (make-vocabulary-entry
			  :name rname	     
			 :word word
			 :remaining-words rest
			 :pos 'w::name
			 :senses (list entry)
			 ))
       olddef))
    ))

;; make-name-entry-with-sem (name class &optional fname)
;; name    : list e.g. (Strong Memorial Hospital)
;; class   : kr-class (i.e. kr-type) to which the result belongs
;; fname   : support for abbreviations -- this is the full name in case "name" is abbreviated
;; returns : word-sense-definition structure
;;
(defun make-name-entry-with-sem (name class &optional fname)
  "Defines a CLASS entry unless it is already there"
  (setq name (util::convert-to-package name :W))
  (multiple-value-bind (word part rest)
      (parse-word-spec name)
    (declare (ignore part))
    (let* ((lf-info (get-lf-info-for-class class))
	   (full-name (or (and fname (util::convert-to-package fname :W)) name))
	   (lf (second lf-info))
	   (entry (make-word-sense-definition
		   :name (gen-id (if rest (cons word rest) word))
		   :pos 'w::name
		   :lf lf
		   :sem (third lf-info)
		   :word word
		   :remaining-words rest
		   :boost-word t ;; this is a note that this should be boosted as necessary
		   :pref 1 ;; actual pref will be determined when actual parser entry is created, based on runtime prefs
		   :transform (first lf-info)
		   :specialized t
		   :kr-type class
		   :syntax `((w::name +) (w::full-name ,full-name) (w::class ,class) (w::agr w::3s))
		   ))
	   ;; this isn't used anywhere & generates a warning Feb 11 2004
	  ;; (fword (if (consp word) (first word) word))	    
	   )
      entry)
    )
  )


;; special handling for streets & their abbreviations
(defun add-street-to-lexicon (s class)
  "Defines a street entry unless it is already there"
  (mapcar #'add-name-to-lexicon (make-street-entries s class))
  )

(defun make-street-entries (s class)
  "Defines a list of entries associated with a street and its possible short names unless they are already there"
  (let* (;; slf gets to be east_ridge_road, the unabbreviated name
	 (sname (reduce (lambda (x y) (read-from-string (format nil "~S_~S" x y))) s))
	 (sentry (make-name-entry-with-sem s class sname))
	 )
    (if (and (> (length s) 1) 
	     (member (first (last s)) '(street st avenue ave lane ln road rd boulevard blvd drive dr)))
	;; create the abbreviations -- just remove last word
	;; define 2 composite names: e.g. (East River Rd) and (East River)
     	(list sentry (make-name-entry-with-sem (butlast s) class sname))
      ;; can't make an abbreviation, just return the basic entry
      (list sentry))
      ;;      ;; else - a 2-word name, so a composite and a simple name
      ;; ;; e.g. (Elmwood Ave) and Elmwood
      ;;(list sentry (make-name-entry-with-sem (first s) class sname))
    ))
  
