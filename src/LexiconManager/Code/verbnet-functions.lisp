;; verbnet-functions.lisp
;;
;; Mary Swift
;;
;; Functions used in experiments for integrating verbnet representations with TRIPS
;;

(in-package "LEXICONMANAGER")


(defun compare-word-lists (wordfile1 wordfile2)
  "
;; 
;; wordlist1 : list of words
;; wordlist2 : list of words in which to search for words in wordlist1, e.g. all the verbs in VerbNet

"
  (let ((res1 nil)
	(res2 nil)
	(wordlist1 (util::read-in-list-from-stream wordfile1))
	(wordlist2 (util::read-in-list-from-stream wordfile2))
	)
    (dolist (w wordlist1)
      (if (member w wordlist2)
	  (pushnew w res1 :test #'equal)
	 (pushnew w res2 :test #'equal))
      )
    (format t "total words from file ~S found in file ~S: ~S~% ~S~%" wordfile1 wordfile2 (length res1) (sort res1 #'string-lessp))
    (format t "total words from file ~S not found in file ~S: ~S~% ~S~%" wordfile1 wordfile2 (length res2) (sort res2 #'string-lessp) )
   )
  )

(defun count-words-and-senses (lexicon-data-file)
  "Get totals for the words and the senses"
  (let* ((sense-count 0)
	 (lexlist (util::read-in-list-from-stream lexicon-data-file))
	 (lex-entries (second (member ':words lexlist)))
	 (word-count (length lex-entries))
	)    
    (dolist (w lex-entries)
      (setf sense-count (+ sense-count (length (second (member 'senses w)))))
      )
    (format t "new words ~S new senses ~S~%" word-count sense-count)
    )  
  )

(defun extract-role-patterns (vn-frames)
  "extract the roles from a verbnet frame representation

  vn-frames  list of verbnet frames, e.g. (((NP agent) (NP theme)) ((NP cause) (NP theme)))
  return     role patterns (the frames without the syntactic categories), e.g. ((agent theme) (cause theme))
"
  (let ((res nil)
	(role-patterns nil)
	)
    (dolist (frame vn-frames)
      (dolist (rolemap frame)
	(if (eq (length rolemap) 2)
	    (push (second rolemap) res)
	    )
	)
      (pushnew (reverse res) role-patterns :test #'equal)
      (setq res nil)
      )
    role-patterns)
  )

(defun match-role-patterns (match-lst role-patterns)
  "
   match-list     list of patterns that are allowed to match a given trips template, e.g. ((agent) (agent theme))
   role-patterns  list of role patterns, e.g. ((agent theme) (cause theme)) extracted from the verbnet frames
   return         t if match is found, otherwise nil

  
"
  (let ((res nil))
    (dolist (pattern role-patterns)
      (dolist (match-pattern match-lst)
	(if (equal pattern match-pattern) (setq res t))
	)
      )
;;    (format t "res is ~S for template list ~S and frames ~S ~%" res match-lst role-patterns)
    res)
  )

(defun match-trips-template-to-vn-frame (trips-templ vn-frames)
  "given a trips template return the corresponding verbnet frame, if any "
  (let ((res nil)
	(vn-role-patterns (extract-role-patterns vn-frames))
	)
    (case trips-templ
      (agent-templ
       (setq res (match-role-patterns '((agent) (agent location)) vn-role-patterns))
       )
      (agent-theme-xp-templ
       (setq res (match-role-patterns '((agent theme)(agent patient)) vn-role-patterns))
       )
      (agent-theme-to-loc-templ
       (setq res (match-role-patterns '((agent theme destination) (agent theme location)) vn-role-patterns))
       )
      (agent-theme-source-optional-templ
       (setq res (match-role-patterns '((agent theme) (agent theme source) (agent theme location)) vn-role-patterns))
       )
      (agent-addressee-templ
       (setq res (match-role-patterns '(agent recipient) vn-role-patterns))
       )
      (agent-addressee-theme-optional-templ
       (setq res (match-role-patterns '(agent recipient) vn-role-patterns))
       )
      (agent-theme-addressee-optional-templ
       (setq res (match-role-patterns '(agent topic) vn-role-patterns))
       )
      (agent-theme-co-theme-optional-templ
       (setq res (match-role-patterns '((agent patient1) (agent patient1 patient2) (agent theme)) vn-role-patterns))
       )
      (experiencer-theme-xp-templ
       (setq res (match-role-patterns '((experiencer theme) (experiencer patient) (experiencer cause)) vn-role-patterns))
       )
      (cognizer-theme-xp-templ
       (setq res (match-role-patterns '((experiencer theme) (experiencer patient) (experiencer cause) (agent theme) (agent patient) (agent cause)) vn-role-patterns))
       )
      (theme-templ
       (setq res (match-role-patterns '((theme) (patient) (theme location)) vn-role-patterns))
       )
      (theme-co-theme-xp-templ
       (setq res (match-role-patterns '(theme1 theme2) vn-role-patterns))
       )
      (theme-source-optional-templ
       (setq res (match-role-patterns '((theme) (theme location)) vn-role-patterns))
       )
      (cause-theme-xp-templ
       (setq res (match-role-patterns '((cause theme) (cause experiencer)) vn-role-patterns))
       )
      (otherwise nil
     ;; (format t "No template match found for ~S in verbnet frame list ~S~%" trips-templ vn-frames)
       )
      )
    res)
  )

(defvar *trips-in-vnet* nil)
(defvar	*trips-not-in-vnet* nil)
(defvar *classes-w-trips* nil) ;; names of the vn classes that have trips exemplars
(defvar *classes-no-trips* nil) ;; names of the vn classes that have no trips exemplar
(defvar *vn-classes* nil) ;; names of all vn classes checked
(defvar *classes-added* nil) ;; names of the vn classes from which we added verbs
(defvar *vn-verbs-added* nil) ;; all the verbs from vn that we added
;;(defvar *verb-senses-added* 0)
(defvar *vn-verbs-in-trips-no-sense-match* nil) ;; all the verbs with exemplars in trips but no sense match
(defvar *vn-verbs-no-template-created* nil)
(defvar *new-vdefs* nil) ;; list of new verb definitions to add to trips -- these get written to a lexicon data file
(defvar *new-lfs* nil)
(defvar *new-templates* nil)
(defvar *found-verb-in-verbnet* nil)
(defvar *verbs-and-senses-added* nil)
(defvar *verb-in-class* nil) ;; t if target verb found in class or its subclasses -- declared as global because search is recursive
(defvar *trips-sense-matches* nil)
(defvar *trips-verbs-with-sense-matches* nil)

(defun initialize-global-vars ()
  (setq *new-vdefs* nil)
;;  (setq *verb-senses-added* 0)
  (setq *vn-verbs-added* nil)
  (setq *vn-verbs-in-trips-no-sense-match* nil)
  (setq *vn-verbs-no-template-created* nil)
  (setq *found-verb-in-verbnet* nil)
  (setq *classes-w-trips* nil)
  (setq *classes-no-trips* nil)
  (setq *vn-classes* nil)
  (setq *classes-added* nil)
  (setq *new-lfs* nil)
  (setq *new-templates* nil)
  (setq *trips-in-vnet* nil)
  (setq *trips-not-in-vnet* nil)
  (setq *verbs-and-senses-added* nil)
  (setq *trips-sense-matches* nil)
  (setq *trips-verbs-with-sense-matches* nil)
  )


;; this needs to be changed to be recursive to handle nested subclasses
 (defun add-verbs-from-verbnet (vnet-class-member-file verblist new-verb-filename)
     "
;; vnet-class-member-file : verbnet classes, members and frames in list format
;; verblist : list of verbs that should be added if they can be matched in vnet
;; new-verb-filename : filename for the new verbs
;; returns  : 
"
  (let ((vn-class-list (util::read-in-list-from-stream vnet-class-member-file)))
    ;; search every verbnet class for members in the list
    (initialize-global-vars)
    (dolist (class vn-class-list)
      (format t "Class is:  ~S ~%" class)
      (find-verbs-in-verbnet class verblist)
      ;; subclasses too
      (dolist (el class)
	(when (and (listp el) (member 'subclasses el))
	  (format t "subclass is:  ~S ~%" el)
	  (dolist (subclass (cdr el))
	    (find-verbs-in-verbnet subclass verblist)
	    )
	  )
	)
      ;; write all the new verb definitions to a lexicon data file
      (create-lexicon-verb-file (reverse *new-vdefs*) new-verb-filename)
      ;; write all the new lfs into an lf-ontology data file
      (write-list-to-file (reverse *new-lfs*) "verbnet-types.lisp")
      (create-template-file *new-templates* "verbnet-templates.lisp")
      (format t "Verbs found in verbnet ~S ~S ~%" (length *found-verb-in-verbnet*) (reverse *found-verb-in-verbnet*))
      (format t "vn classes added ~S ~S ~%" (length *classes-added*) (reverse *classes-added*))
      (format t "new verbs added ~S ~S ~%" (length *vn-verbs-added*) (reverse *vn-verbs-added*))
      (format t "new templates added ~S ~%" (length *new-templates*))
      (format t "new lfs added ~S ~%" (length *new-lfs*))
;;      (format t "new senses added ~A ~%" *verb-senses-added*)
      )
    )
  )


(defun find-verbs-in-verbnet (vn-class vlist)
  (let ((class-type (first vn-class))
	(vn-class-name (second vn-class))	
	(vn-member-list nil)
	(vn-frames-list nil)
	)
    ;; find the members and frames lists for this class - they are not always there or always in the same place so we have to search
    (dolist (el2 vn-class)
      (when (listp el2)
	(cond
	 ((member 'FRAMES el2) (setq vn-frames-list (cdr el2)))
	 ((member 'MEMBERS el2) (setq vn-member-list (cdr el2)))
	 (t nil)
	 )
	)
      )
    (format t "~S:  ~S ~% Members:  ~S ~%" class-type vn-class-name vn-member-list)
    (dolist (w vn-member-list)
	(cond ((member w vlist)
	       (pushnew w *found-verb-in-verbnet*)
	       (create-vn-verb-definition-from-vn w vn-class-name vn-frames-list)
	       )
	      (t nil)
	      )
	)
    )
  )
 
(defun find-trips-in-vnet (vn-class outfile)
  "check all verbs (members) in this class for a match in TRIPS"
  (let ((class-type (first vn-class))
	(vn-class-name (second vn-class))	
	(vn-member-list nil)
	(vn-frames-list nil)
	(vn-members-not-in-trips nil)
	(vn-members-in-trips nil)
	)
    ;; find the members and frames lists for this class - they are not always there or always in the same place so we have to search
    (dolist (el2 vn-class)
      (when (listp el2)
	(cond
	 ((member 'FRAMES el2) (setq vn-frames-list (cdr el2)))
	 ((member 'MEMBERS el2) (setq vn-member-list (cdr el2)))
	 (t nil)
	 )
	)
      )
    (format outfile "~S:  ~S ~% Members:  ~S ~%" class-type vn-class-name vn-member-list)
    (dolist (w vn-member-list)
      (let* ((ww (util::convert-to-package w 'w)) ;; words must have the w package prefix
	     (wdef (retrieve-from-lex ww))
	     )
	(cond ((not wdef) ;; word is not defined in TRIPS 
	       (pushnew w *trips-not-in-vnet* :test #'equal)
	       (pushnew w vn-members-not-in-trips :test #'equal)
	       )
	      ((or (not (eq (lex-entry-cat (car wdef)) 'w::v)) ;; word is defined in TRIPS, but not as a verb
		   (and (eq (lex-entry-cat (car wdef)) 'w::v)  ;; word is defined in TRIPS but as an aux verb (closed class)
			(eq (get-feature-values (lex-entry-feats (car wdef)) 'w::aux) '+)))
	       (pushnew w *trips-not-in-vnet* :test #'equal) 
	       (pushnew w vn-members-not-in-trips :test #'equal)
	       )
	      ;; otherwise the verb is defined in TRIPS -- create a new entry if there is a sense match
	      (t
	       (pushnew w *trips-in-vnet* :test #'equal)
	       (pushnew w vn-members-in-trips :test #'equal)
	       (pushnew vn-class-name *classes-w-trips* :test #'equal)
	       ;; create lexical definitions for all verb members in this class not already in TRIPS
	       (dolist (vn-v vn-member-list)
		 (let* ((wvn-v (util::convert-to-package vn-v 'w)) ;; words must have the w package prefix
			(new-vdef (retrieve-from-lex wvn-v)) ;; check if already defined
			)
		   (when (not (and new-vdef (member vn-class-name *classes-added*)))
		     (create-vn-verb-definition-from-trips-exemplar ww wvn-v vn-v vn-class-name vn-frames-list outfile)
		     ))))
	      )
	)
      )
    ;; if no matching trips members found in this class, add class to list of classes without trips exemplars
    (if (not vn-members-in-trips) (pushnew vn-class-name *classes-no-trips* :test #'equal))
;;    (format outfile "~S verbs not defined in TRIPS ~S ~S ~%" vn-class-name (length vn-members-not-in-trips) (reverse vn-members-not-in-trips))
    (format outfile "~S verbs in TRIPS ~S ~S ~%" vn-class-name (length vn-members-in-trips) (reverse vn-members-in-trips))
    )
  )

(defun add-if-match-in-trips (verb vn-class outfile)
  "check verbs in this class for a match in TRIPS"
  (let (
	(vn-class-name (second vn-class))	
	(vn-member-list nil)
	(vn-frames-list nil)
	(vn-members-not-in-trips nil)
	(vn-members-in-trips nil)
	)
    ;; find the members and frames lists for this class - they are not always there or always in the same place so we have to search
    (dolist (el vn-class)
      (when (listp el)
	(cond
	 ((member 'FRAMES el) (setq vn-frames-list (cdr el)))
	 ((member 'MEMBERS el) (setq vn-member-list (cdr el)))
	 (t nil)
	 )
	)
      )
    (dolist (w vn-member-list)
      (let* ((ww (util::convert-to-package w 'w)) ;; words must have the w package prefix
	     (wdef (retrieve-from-lex ww))
	     )
	(cond ((not wdef) ;; word is not defined in TRIPS 
	       (pushnew w *trips-not-in-vnet* :test #'equal)
	       (pushnew w vn-members-not-in-trips :test #'equal)
	       )
	      ((or (not (eq (lex-entry-cat (car wdef)) 'w::v)) ;; word is defined in TRIPS, but not as a verb
		   (and (eq (lex-entry-cat (car wdef)) 'w::v)  ;; word is defined in TRIPS but as an aux verb (closed class)
			(eq (get-feature-values (lex-entry-feats (car wdef)) 'w::aux) '+)))
	       (pushnew w *trips-not-in-vnet* :test #'equal) 
	       (pushnew w vn-members-not-in-trips :test #'equal)
	       )
	      ;; otherwise the verb is defined in TRIPS -- create a new entry if there is a sense match
	      (t
	       (pushnew w *trips-in-vnet* :test #'equal)
	       (pushnew w vn-members-in-trips :test #'equal)
	       (pushnew vn-class-name *classes-w-trips* :test #'equal)
	       ;; create lexical definitions for all verb members in this class not already in TRIPS
;;	       (dolist (vn-v vn-member-list)
	       ;; only do this for the specific verb
	       (let* ((wvn-v (util::convert-to-package verb 'w)) ;; words must have the w package prefix
		      (new-vdef (retrieve-from-lex wvn-v)) ;; check if already defined
		      )
		 (when (not (and new-vdef (member vn-class-name *classes-added*)))
		   (create-vn-verb-definition-from-trips-exemplar ww wvn-v verb vn-class-name vn-frames-list outfile)
		   ))
		;; )
	       )
	      )
	)
      )
    ;; if no matching trips members found in this class, add class to list of classes without trips exemplars
    (if (not vn-members-in-trips) (pushnew vn-class-name *classes-no-trips* :test #'equal))
;;    (format outfile "~S verbs not defined in TRIPS ~S ~S ~%" vn-class-name (length vn-members-not-in-trips) (reverse vn-members-not-in-trips))
    (format outfile "~S verbs in TRIPS ~S ~S ~%" vn-class-name (length vn-members-in-trips) (reverse vn-members-in-trips))
    )
  )
 (defun find-trips-match (verb verbnet-classes outfile)
    "
;; verbnet-classes : verbnet classes, members and frames in list format
;; outfile : filename for recording output of this process
;; returns  : list of trips verbs found in each class
"
   ;; check if verbs in this class match verbs already defined in trips
   (dolist (class verbnet-classes)
;;     (format outfile "Class is:  ~S ~%" class)
     (add-if-match-in-trips verb class outfile)
     (pushnew (second class) *vn-classes*)
     ;; subclasses too
     (dolist (el class)
       (when (and (listp el) (member 'subclasses el))
	 (find-trips-match verb (cdr el) outfile)
	  )
	)
      )
  )


 (defun get-verbnet-verbs-in-trips (verbnet-classes outfile)
    "
;; verbnet-classes : verbnet classes, members and frames in list format
;; outfile : filename for recording output of this process
;; returns  : list of trips verbs found in each class
"
   ;; check if verbs in this class match verbs already defined in trips
   (dolist (class verbnet-classes)
     (format outfile "Class is:  ~S ~%" class)
     (find-trips-in-vnet class outfile)
     (pushnew (second class) *vn-classes*)
     ;; subclasses too
     (dolist (el class)
       (when (and (listp el) (member 'subclasses el))
	 (get-verbnet-verbs-in-trips (cdr el) outfile)
	  )
	)
      )
  )

(defun find-verb-in-class (verb class)
  "
;; verb    : target verb to match
;; class   : verbnet class, containing a class name, member list (the verbs) and frames list
;; returns : t if verb found in class or its subclasses; otherwise nil
;; uses global variable *verb-in-class*
"
  (when (listp class)
    (dolist (el class)
      (when (listp el)
	;; get the verbs for this class
	(if (member 'MEMBERS el)
	    (if (find verb el) (setq *verb-in-class* t)))
	;; if we haven't found the verb yet, search the subclasses
	;; (sometimes they are embedded so we have two tests)
	(if (not *verb-in-class*)
	    (if (or (member 'subclasses el)
		      (member 'vnsubclass el))
	      (find-verb-in-class verb (cdr el))))
	)))
  *verb-in-class*)

(defun process-verbnet-verbs-in-verbnet (vnet-class-member-file verblist new-verb-filename)
   "
Top-level function for matching verbnet verb definitions with verbs already defined in TRIPS
but limited only to the verbs in verblist. New verb definitions are created for all matched verbs.
;; vnet-class-member-file : verbnet classes, members and frames in list format
;; verblist               : list of verbs to add if found in verbnet
;; new-verb-filename      : filename for the new verbs
;; returns                : creates two files,
;;                          the new verb definitions in new-verb-filename,
;;                          and processing info in new-verb-filename-out 
"
   (let ((vn-class-list (util::read-in-list-from-stream vnet-class-member-file))
	 (outfile (open (concatenate 'string new-verb-filename "-out") :direction :output
			:if-exists :supersede))
	 )
     (initialize-global-vars)
       (dolist (class vn-class-list)
	 (dolist (verb verblist)
	 (setq *verb-in-class* nil)
	 ;; if the verb is in the vn-class
	 (when (find-verb-in-class verb class)
	   (format outfile "Verb ~S found in class ~S~%" verb class)
	     ;; then search this class for matching verbs in TRIPS
	     ;; and generate new verb definitions for any matches found
	     (find-trips-match verb (list class) outfile))
	 )
       )
     ;; write all the new verb definitions to a lexicon data file
     (create-lexicon-verb-file (reverse *new-vdefs*) new-verb-filename)
     ;; and write out the processing information to outfile
     (format outfile "~%~%************ SUMMARY *************~%" )
  ;;   (format outfile "Total Verbnet verbs not defined in TRIPS ~S ~S ~%" (length *trips-not-in-vnet*) (sort *trips-not-in-vnet* #'string-lessp))
     (format outfile "Total TRIPS verbs found in Verbnet ~S ~S ~%" (length *trips-in-vnet*) (sort *trips-in-vnet* #'string-lessp))
     (format outfile "Total TRIPS verbs with sense matches ~S ~S ~%" (length *trips-verbs-with-sense-matches*) (sort *trips-verbs-with-sense-matches* #'string-lessp))
      (format outfile "Total TRIPS sense matches ~S ~S ~%" (length *trips-sense-matches*) (sort *trips-sense-matches* #'string-lessp :key #'car))
   ;;  (format outfile "Verb classes with no TRIPS exemplar ~S ~S ~%" (length *classes-no-trips*) (sort *classes-no-trips* #'string-lessp))
     (format outfile "Verb classes with a TRIPS exemplar ~S ~S ~%" (length *classes-w-trips*) (sort *classes-w-trips* #'string-lessp))
     (format outfile "no sense match found for ~S ~S ~%" (length *vn-verbs-in-trips-no-sense-match*)
	    (sort *vn-verbs-in-trips-no-sense-match* #'string-lessp :key #'cadr))
     (format outfile "vn classes added ~S ~S ~%" (length *classes-added*) (sort *classes-added* #'string-lessp))
     (format outfile "total classes checked ~S ~S ~%" (length *vn-classes*) (sort *vn-classes* #'string-lessp))
     (format outfile "new verbs added ~S ~S ~%" (length *vn-verbs-added*) (sort *vn-verbs-added* #'string-lessp))
     (format outfile "verb-sense pairs added ~S ~%" (sort *verbs-and-senses-added* #'string-lessp :key #'car))
     (close outfile)
    )
  )



(defun process-verbnet-verbs-in-trips (vnet-class-member-file verblist new-verb-filename)
   "
Top-level function for matching verbnet verb definitions with verbs already defined in TRIPS
but limited only to the verbs in verblist. New verb definitions are created for all matched verbs.
;; vnet-class-member-file : verbnet classes, members and frames in list format
;; verblist               : list of verbs to add if found in verbnet
;; new-verb-filename      : filename for the new verbs
;; returns                : creates two files,
;;                          the new verb definitions in new-verb-filename,
;;                          and processing info in new-verb-filename-out 
"
   (let ((vn-class-list (util::read-in-list-from-stream vnet-class-member-file))
	 (outfile (open (concatenate 'string new-verb-filename "-out") :direction :output
			:if-exists :supersede))
	 )
     (initialize-global-vars)
       (dolist (class vn-class-list)
	 (dolist (verb verblist)
	 (setq *verb-in-class* nil)
	 ;; if the verb is in the vn-class
	 (when (find-verb-in-class verb class)
	   (format outfile "Verb ~S found in class ~S~%" verb class)
	     ;; then search this class for matching verbs in TRIPS
	     ;; and generate new verb definitions for any matches found
	     (find-trips-match verb (list class) outfile))
	 )
       )
     ;; write all the new verb definitions to a lexicon data file
     (create-lexicon-verb-file (reverse *new-vdefs*) new-verb-filename)
     ;; and write out the processing information to outfile
     (format outfile "~%~%************ SUMMARY *************~%" )
  ;;   (format outfile "Total Verbnet verbs not defined in TRIPS ~S ~S ~%" (length *trips-not-in-vnet*) (sort *trips-not-in-vnet* #'string-lessp))
     (format outfile "Total TRIPS verbs found in Verbnet ~S ~S ~%" (length *trips-in-vnet*) (sort *trips-in-vnet* #'string-lessp))
     (format outfile "Total TRIPS verbs with sense matches ~S ~S ~%" (length *trips-verbs-with-sense-matches*) (sort *trips-verbs-with-sense-matches* #'string-lessp))
      (format outfile "Total TRIPS sense matches ~S ~S ~%" (length *trips-sense-matches*) (sort *trips-sense-matches* #'string-lessp :key #'car))
   ;;  (format outfile "Verb classes with no TRIPS exemplar ~S ~S ~%" (length *classes-no-trips*) (sort *classes-no-trips* #'string-lessp))
     (format outfile "Verb classes with a TRIPS exemplar ~S ~S ~%" (length *classes-w-trips*) (sort *classes-w-trips* #'string-lessp))
     (format outfile "no sense match found for ~S ~S ~%" (length *vn-verbs-in-trips-no-sense-match*)
	    (sort *vn-verbs-in-trips-no-sense-match* #'string-lessp :key #'cadr))
     (format outfile "vn classes added ~S ~S ~%" (length *classes-added*) (sort *classes-added* #'string-lessp))
     (format outfile "total classes checked ~S ~S ~%" (length *vn-classes*) (sort *vn-classes* #'string-lessp))
     (format outfile "new verbs added ~S ~S ~%" (length *vn-verbs-added*) (sort *vn-verbs-added* #'string-lessp))
     (format outfile "verb-sense pairs added ~S ~%" (sort *verbs-and-senses-added* #'string-lessp :key #'car))
     (close outfile)
    )
  )

(defun process-all-verbnet-verbs-in-trips (vnet-class-member-file new-verb-filename)
   "
Top-level function for matching verbnet verb definitions with verbs already defined in TRIPS
;; vnet-class-member-file : verbnet classes, members and frames in list format.
;; New verb definitions are created for all matched verbs.
;; new-verb-filename      : filename for the new verbs
;; returns                : creates two files,
;;                          the new verb definitions in new-verb-filename,
;;                          and processing info in new-verb-filename-out 
"
   (let ((vn-class-list (util::read-in-list-from-stream vnet-class-member-file))
	 (outfile (open (concatenate 'string new-verb-filename "-out") :direction :output
			:if-exists :supersede))
	 )
     (initialize-global-vars)
     ;; search all verbnet classes (and subclasses) for matching verbs in TRIPS
     (get-verbnet-verbs-in-trips vn-class-list outfile)
     
     ;; write all the new verb definitions to a lexicon data file
     (create-lexicon-verb-file (reverse *new-vdefs*) new-verb-filename)
     ;; and write out the processing information to outfile
     (format outfile "~%~%************ SUMMARY *************~%" )
     (format outfile "Total Verbnet verbs not defined in TRIPS ~S ~S ~%" (length *trips-not-in-vnet*) (sort *trips-not-in-vnet* #'string-lessp))
     (format outfile "Total TRIPS verbs found in Verbnet ~S ~S ~%" (length *trips-in-vnet*) (sort *trips-in-vnet* #'string-lessp))
     (format outfile "Verb classes with no TRIPS exemplar ~S ~S ~%" (length *classes-no-trips*) (sort *classes-no-trips* #'string-lessp))
     (format outfile "Verb classes with a TRIPS exemplar ~S ~S ~%" (length *classes-w-trips*) (sort *classes-w-trips* #'string-lessp))
     (format outfile "no sense match found for ~S ~S ~%" (length *vn-verbs-in-trips-no-sense-match*)
	    (sort *vn-verbs-in-trips-no-sense-match* #'string-lessp :key #'cadr))
     (format outfile "vn classes added ~S ~S ~%" (length *classes-added*) (sort *classes-added* #'string-lessp))
     (format outfile "total classes checked ~S ~S ~%" (length *vn-classes*) (sort *vn-classes* #'string-lessp))
     (format outfile "new verbs added ~S ~S ~%" (length *vn-verbs-added*) (sort *vn-verbs-added* #'string-lessp))
     (format outfile "verb-sense pairs added ~S ~%" (sort *verbs-and-senses-added* #'string-lessp :key #'car))
     (close outfile)
    )
  )

(defun create-lexicon-verb-file (lst filename)
  "Print a list to a file"
  (let  ((outstream (open filename :direction :output
			  :if-exists :supersede :if-does-not-exist :create)))
    ;; use format statements to get rid of lxm package prefix
    (setf lst (read-from-string (stringify `(define-words :pos w::v :templ agent-theme-xp-templ :words ,lst))))
    (print lst outstream)
    (close outstream)
    )
  )


(defun create-template-file (lst filename)
  "Print a list to a file"
  (let  ((outstream (open filename :direction :output
			  :if-exists :supersede :if-does-not-exist :create)))
    ;; use format statements to get rid of lxm package prefix
    (setf lst (read-from-string (stringify `(define-templates  ,lst))))
    (print lst outstream)
    (close outstream)
    )
  )

(defun write-list-to-file (lst filename)
  "Print a list to a file"
  (let  ((outstream (open filename :direction :output
			  :if-exists :supersede :if-does-not-exist :create)))
    (print lst outstream) 
    (close outstream)
    )
  )

(defun get-senses (wdef)
  "return the sense list from a word definition"
  (third (car wdef))
  )

(defun universal-time-to-timestamp (tm)
  "Converts from the internal time to timestamp"
  (cond
   ((eql tm 0)
    nil)
   ((null tm)
    nil)
   ;; if already in timestamp format, just return it
   ((listp tm)
    tm)
   (t
    (multiple-value-bind
	(sec min hour day month year)
	(decode-universal-time tm)
      (list month day year hour min sec)
      ))
   ))


(defun create-vn-verb-definition-from-trips-exemplar (trips-verb wvn-verb vn-v vn-class-name vn-frames outfile)
  "create a lexical entry for a verb based on a verb already defined in trips"
  (let* ((new-lf nil)
	 (new-templ nil)
	 (new-meta-data nil)
	 (new-senses-list nil)
	 (new-sense nil)
	 (trips-def (getwordDefs trips-verb *lexicon-data*))
	 (trips-senses (get-senses trips-def))
	)
    ;; check TRIPS senses for a  matching syntax-sematic mapping
    (dolist (s (vocabulary-entry-senses trips-senses))
      (let ((tmpl (sense-definition-templ s)) 
	    (params (sense-definition-params s))
	    )
	(format outfile "~S has template ~S ~S ~%" trips-verb tmpl params)
	;; if we have a template match (= sense match for us), create a new definition based on this sense
	(when (match-trips-template-to-vn-frame tmpl vn-frames)
	  (format outfile "match occurred with verb ~S for sense ~S ~S" trips-verb s tmpl)
	  ;; include the params list, if any. Note that this doesn't work for the from-to templ
	  (if params
	      (setf new-templ (list 'templ tmpl (car (sense-definition-params s))))
	    (setf new-templ (list 'templ tmpl)))
	  (setf new-lf (list 'lf-parent (sense-definition-lf-parent s)))
	  (setf new-meta-data `(meta-data :origin verbnet-from-trips
					  :entry-date ,(universal-time-to-timestamp (get-universal-time))
					  :change-date nil
					  :comments ,vn-class-name))
	  (setf new-sense (list new-lf new-templ new-meta-data))
	  (pushnew trips-verb *trips-verbs-with-sense-matches* :test #'equal)
	  (pushnew (list trips-verb (sense-definition-lf-parent s) vn-v) *trips-sense-matches* :test #'equal)
	  (pushnew (list vn-v (sense-definition-lf-parent s)) *verbs-and-senses-added* :test #'equal)
	  (pushnew new-sense new-senses-list :test #'equal)
	  )
	)
      )
    ;; if senses were defined, add the new entry and/or senses to the list of new verb definitions
    (cond (new-senses-list
	   (add-new-senses-to-vdefs wvn-verb new-senses-list)
;;	   (setf *verb-senses-added* (+ (length new-senses-list) *verb-senses-added*))
	   (pushnew vn-v *vn-verbs-added* :test #'equal)
	   (pushnew vn-class-name *classes-added* :test #'equal)
	   )
	  (t
	   (pushnew (list trips-verb vn-v vn-class-name) *vn-verbs-in-trips-no-sense-match* :test #'equal))
	  )
    )
  )

(defun get-role-names (frames)
  (let ((roles nil))
    (dolist (frame frames)  ;; for each frame
      (dolist (role frame)  ;; for each role in the frame
	(pushnew role roles :test #'equal) ;; add it to the list
	)
      )
    roles)
  )

(defun generate-lf-arg-list (frames)
  (let ((roles (get-role-names frames))
	(arg-list nil))
    (dolist (role roles)
      (let ((lf-role (util::convert-to-package role 'ont))) ;; lf roles must have the ont package prefix
	(setq arg-list (pushnew `(:essential ,lf-role) arg-list :test #'equal))
	)
      )
    arg-list)
  )

(defun create-vn-lf-type (name frames)
  "generate a trips lf structure from verbnet class name and frames"
  (let ((lf-arg-list (generate-lf-arg-list frames))
	(new-lf-type nil)
	)
    (setf new-lf-type `(define-type ,name
			 :parent lf::situation-root
			 :arguments ,lf-arg-list)
	  )
    (pushnew new-lf-type *new-lfs* :test #'equal)
    )
  )


;; a sample frame: ((NP agent) (NP theme) (PREP on onto) (NP destination))
;; should return ((NP agent) (NP theme) (PREP (on onto) destination))
(defun replace-double-arguments (frame)
  (let ((res nil)
	(get-next-arg nil)
	(double-arg nil)
	)
    (dolist (arg frame)
      (let ((id (first arg)))
	(cond
	 ;; check for 'double' argument representation -- right now only prepositions or sentential arguments
	 ((and (not get-next-arg) (or (eq id 'prep) (eq id 's-comp)))
	  (setf get-next-arg t)
	  (push (list id (rest arg)) double-arg)
	  )
	 ;; get the second part of a double argument representation
	 (get-next-arg
	  (setf get-next-arg nil)
	  (push (list (caar double-arg) (cadar double-arg) (second arg)) res)
	  )
	 ;; otherwise we have a simple role, just put it on the list
	 (t (push arg res)) 
	 )   
	)
      )
    (reverse res))
  )


(defun get-role-name (arg)
  ;; arg should either be (cat rolename) or (cat () rolename)
  ;; e.g. (NP AGENT) or (PP (in at) LOCATION)
  (case (length arg)
    (2 (second arg))
    (3 (third arg))
    (t nil)
    )
  )

(defun make-synsem-mapping (arg position)
  (let ((role (util::convert-to-package (get-role-name arg) 'ont))
	(res nil)
	)
    (case (first arg)
      (NP       
       (setq res `(,position (:parameter xp (:default (% W::NP))) ,role))
       )
      (PREP
       (setq res `(,position (:parameter xp (:default (% W::PP))) ,role))
       )
      (S-COMP
       (setq res `(,position (:parameter xp (:default (% W::CP))) ,role))
       )
      (t nil)
      )
   res)
  )

(defun create-trips-template-from-vn-frame (v frame)
  (let* ((template-name nil)
	 (args (replace-double-arguments frame))
	 (new-template nil)
	 (arg1 nil)
	 (arg2 nil)
	 (arg3 nil)
	 )
    (case (length args)
      (1
       (setq arg1 (get-role-name (first args)))
       (setq template-name (read-from-string (concatenate 'string (princ-to-string arg1) "-templ")))
       (setq new-template `(,template-name
			    (arguments ,(make-synsem-mapping (first args) 'lsubj)
			     ))
	     )
       )
      (2
       (setq arg1 (get-role-name (first args)))
       (setq arg2 (get-role-name (second args)))
       (setq template-name (read-from-string (concatenate 'string (princ-to-string arg1) "-" (princ-to-string arg2) "-xp-templ")))
       (setq new-template `(,template-name
			    (arguments
			     ,(make-synsem-mapping (first args) 'lsubj)
			     ,(make-synsem-mapping (second args) 'lobj)
			     ))
               )
         )
      (3
       (setq arg1 (get-role-name (first args)))
       (setq arg2 (get-role-name (second args)))
       (setq arg3 (get-role-name (third args)))
       (setq template-name (read-from-string
			    (concatenate 'string (princ-to-string arg1) "-" (princ-to-string arg2) "-" (princ-to-string arg3) "-xp-templ")))
       (setq new-template `(,template-name
			    (arguments
			     ,(make-synsem-mapping (first args) 'lsubj)
			     ,(make-synsem-mapping (second args) 'lobj)
			     ,(make-synsem-mapping (third args) 'lcomp)
			     ))
               )
         )
      (t
       (format t "~S  no template for frame ~S~%" v args)) ;; can't handle templates with more than three args in trips
      )
    (setq *new-templates* (pushnew new-template *new-templates* :test #'equal))
    template-name)
  )

(defun create-vn-verb-definition-from-vn (vn-v vn-class-name vn-frames)
  (let* ((new-lf nil)
	 (new-lf-name (util::convert-to-package vn-class-name 'ont)) ;; lfs must have the ont package prefix
	 (wvn-v (util::convert-to-package vn-v 'w)) ;; words must have the w package prefix
	 (new-templ nil)
	 (new-meta-data nil)
	 (new-senses-list nil)
	 (new-sense nil)
	)
    ;; generate senses for each frame
    (dolist (frame vn-frames)
      (when (setq new-templ (create-trips-template-from-vn-frame vn-v frame))
	  (create-vn-lf-type new-lf-name vn-frames)
	  (setf new-templ (list 'templ new-templ))
	  (setf new-lf (list 'lf-parent new-lf-name))
	  (setf new-meta-data `(meta-data :origin verbnet-from-verbnet
					  :entry-date ,(universal-time-to-timestamp (get-universal-time))
					  :change-date nil
					  :comments ,vn-class-name))
	  (setf new-sense (list new-lf new-templ new-meta-data))
	  (pushnew new-sense new-senses-list :test #'equal)
	  )
	)
    ;; if senses were defined, add the new entry and/or senses to the list of new verb definitions
    (cond (new-senses-list
	   (add-new-senses-to-vdefs wvn-v new-senses-list)
;;	   (setf *verb-senses-added* (+ (length new-senses-list) *verb-senses-added*))
	   (pushnew vn-v *vn-verbs-added* :test #'equal)
	   (pushnew vn-class-name *classes-added* :test #'equal)
	   )
	  (t  (pushnew vn-v *vn-verbs-no-template-created* :test #'equal))
	  )
    )
  )

(defun get-verb-entry-from-vdefs (v)
  (let ((res nil))
    (dolist (vdef *new-vdefs*)
      (if (member v vdef)
	  (setq res vdef))
      )
;;    (format t "get-verb-from-vdefs returning ~S ~%" res)
    res)
  )

(defun add-senses-to-defined-word (v vdef sense-list)
  (let ((senses (cdr (second vdef)))
	(new-vdef nil)
	)
    ;; add the new senses to the old sense list
    (dolist (new-sense sense-list)
      (setq senses (pushnew new-sense senses :test #'equal)))
    (setf senses (cons 'senses senses))
    (setf new-vdef (list v senses))
    ;; substitute the new entry with augmented senses for the old one
    (setq *new-vdefs* (subst new-vdef vdef *new-vdefs* :test #'equal))
   )
  )

(defun add-new-senses-to-vdefs (v sense-list)
  (let ((vdef (get-verb-entry-from-vdefs v))
	(new-entry nil)
	)
    (cond (vdef
	   (add-senses-to-defined-word v vdef sense-list))
	  (t
	   (setf sense-list (cons 'senses sense-list))
	   (setf new-entry (list v sense-list))
	   (pushnew new-entry *new-vdefs* :test #'equal)
	   )
	  )
    )
  )
      
