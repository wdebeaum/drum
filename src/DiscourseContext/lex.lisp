;;;;
;;;; File: lex.lisp
;;;; Creator: George Ferguson
;;;; Created: Mon Feb 12 15:43:01 2007
;;;; Time-stamp: <Thu Feb 21 15:58:27 EST 2008 swift>
;;;;

(in-package :dc)

(defun get-latest-triple-for-lftype (lftype &key exact-match)
  "Scan all acts in *ACTS* for a word with the given LFTYPE. Return the first
lf triple (:* lftype w) from the first such act for which a unifying match is found, or NIL if no unifying match is found in any of the *ACTS*."
  (some #'(lambda (act)
	    (get-triple-for-lftype-in-act lftype act :exact-match exact-match)) *acts*))

(defun exclude-types (list-of-types)
  "high-level types to omit from the search"
  (mapcar #'(lambda (type)
	      (if (find type list-of-types) (setq list-of-types (remove type list-of-types))
		)
	      )
	  '(ont::root ont::any-sem ont::referential-sem ont::phys-object ont::situation-root ont::abstract-object ont::publication ont::info-medium ont::info-holder ont::phys-representation ont::transfer-information ont::symbolic-representation ont::text-representation ont::ordered-domain ont::domain ont::predicate ont::person))
  list-of-types)

(defun get-triple-for-lftype-in-act (lftype act &key exact-match)
  "Scan the given ACT for a word with the given LFTYPE. Check subtypes and supertypes if exact match is not requested. Return first match found as a triple if the word exists: (:* LFTYPE WORD), or just the LFTYPE, and include :name-of if it exists.
"
  (when (not (member lftype '(ont::have-property ont::in-relation ont::exists ont::person ont::communication))) ;; ignore light verbs like have, be, say/tell
    (if exact-match  ;; if t, then require an exact match of the lftypes. GM calls for the taskgui use this option.
	(remove-if #'null
		   (mapcar #'(lambda (term)
			       (let* ((this-lfterm (get-keyword-arg term :lf))
				      (this-lfterm-id (first this-lfterm))
				      ;; 2 possibilities for the lftype -- inside a triple (:* lftype word)
				      ;; or just the lftype; names from TT come in like that, e.g. (the var lftype :name-of (w::name))	      
				      (this-lftriple (if (listp (third this-lfterm)) (third this-lfterm)))
				      (this-lftype (if this-lftriple (second this-lftriple) (third this-lfterm)))
				      (this-word (if this-lftriple (third this-lftriple)))
				      )
				 (case this-lfterm-id
				   ;; only check terms for content words: nouns, verbs, adj, adv
				   ((ont::a ont::the ont::bare ont::kind ont::f)			      
				    (print-debug "*** checking for exact match lftype ~S with ~S~%" lftype this-lftype)
				    (when (eq this-lftype lftype) 
				      (let ((name-of (member ':name-of this-lfterm))
					    (lfpiece (or this-lftriple this-lftype)))
					(if name-of (cons lfpiece name-of)
					  lfpiece)
				      )))
				 ;; don't check e.g., quantifier and pronoun terms
				 (otherwise nil))
			       ))
		   (get-keyword-arg act :terms)))
      ;; otherwise walk the hierarchy if needed to find a match
      ;; fall 2009: Note that this option may no longer be used now that we don't use transforms for lftype conversion 
      ;; This code checks only terms with an lftriple (:* lftype word), and should be updated as above if needed to check the non-triple lftype e.g. (THE var LFTYPE :name-of (w::name))
    (let ((parents (exclude-types (om::get-parents lftype)))) ;; don't match general top-level lftypes
      (remove-if #'null
		 (mapcar #'(lambda (term)
			     (let* ((lf (get-keyword-arg term :lf)))			   
			       ;; check for an lf triple e.g.  (:* ONT::PURCHASE W::BUY)
			       (if (listp (third lf))
				   (case (first lf)
				     ;; only check words from argument and relation terms
				     ((ont::a ont::the ont::bare ont::kind ont::f)			      
				      (let ((subtype (om::is-sublf (second (third lf)) lftype))
					    (supertype (find (second (third lf)) parents))
					    (thisword (third (third lf)))
					    )				   
					(print-debug "*** checking lftype ~S with ~S~%" lftype (second (third lf)))
					(print-debug "*** subtype is ~S  parents are ~S~%" subtype parents)
					;; check if it's an identical or subordinate concept
					(cond
					 (subtype
					  ;; if this term has a name, return it too
					  (if (member ':name-of lf)
					      (cons (list ':* subtype thisword) (member ':name-of lf))
					    (list ':* subtype thisword))
					  )
					 ;; check for a match among parents
					 (supertype
					; if this term has a name, return it too
					  (if  (member ':name-of lf)
					      (cons (list ':* supertype thisword) (member ':name-of lf))
					    (list ':* supertype thisword))
					  )
					 (t nil))))	     
				     ;; don't check pronoun or quantifier terms
				     (otherwise nil))
				 )))			      
			 (get-keyword-arg act :terms)))
		 )	      
      )
    )
  )



