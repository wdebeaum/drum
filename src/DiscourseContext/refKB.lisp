;;  This has code that builds and queries KB structures build from LF forms
;;     supporting matching using the LF type hierarchy

;;  This supports matching sets of atomic formulas, allowing for hierarchical matching on
;;    the variable TYPES

(in-package :dc)

;; building reference KB  - map an LF to a list of triples

(defun build-refKB (lfs speaker addressee)
  (remove-if #'(lambda (x) (eq (car x) :suchthat)) ;; suchthat expressions are needed to build queries but not needed in KB
	  (mapcan #'(lambda (lf) (build-refKB-for-one lf speaker addressee)) lfs)))

(defun build-refKB-for-one (lf speaker addressee)
  (let ((v (second lf)))
    (if (or (not (eq (car lf) 'ONT::PRO)) (not (member (find-arg (cdddr lf) :context-rel) '(ONT::MY ONT::YOUR ONT::ME ONT::YOU ONT::I))))
	(list* ;;(list 'status v (first lf))  the status is irrelevant for KB matching -- we capture the effects of this elsewhere
	 (list 'type v (third lf))
	 (gen-role-terms (cdddr lf) v))
	;;  we have to special case the "my" and "your" modifiers
	(list (list 'type v (if (symbolp (third lf)) (third lf) (cadr (third lf))))
	      (list :context-rel v 
		    (case (find-arg (cdddr lf) :context-rel)
		      ((ONT::I ONT::ME ONT::MY) speaker)
		      ((ONT::you ONT::Your) addressee)
		      )))
       )
    )
)
	      
(defun gen-role-terms (roles var)
    (when roles
      (append
       (case (car roles)
	 (:TMA (gen-tma-terms (cadr roles) var))
	 (:MODS (if (consp (cadr roles))
		    (mapcar #'(lambda (x) (list :MOD var x)) (cadr roles))
		    (list (list :MOD var (cadr roles)))))
	  
	 (otherwise (list (list (car roles) var (cadr roles)))))
       (gen-role-terms (cddr roles) var))))

(defun gen-tma-terms (terms var)
  (mapcar #'(lambda (x) (list (car x) var (second x)))
	  terms))

;;   Build a query by mapping an LF to a list of triples with variables 

(defun build-ref-query (lfs speaker addressee)
  (multiple-value-bind (qlfs mappings)
      (Add-questionmark-to-vars lfs)
    (values (build-refKB qlfs speaker addressee) mappings)))

(defun add-questionmark-to-vars (lfs)
  (let* ((mappings (mapcar #'gen-var-pair lfs)))
    (values (subst-mappings lfs mappings) mappings))
  )

(defun gen-var-pair (lf)
  (list (second lf) (parser::build-var (second lf) nil)))

(defun subst-mappings (lfs mappings)
  (if  mappings
       (let ((m (car mappings)))
      (subst-mappings (subst (cadr m) (car m) lfs) (cdr mappings)))
    lfs))

;;   Match a query (list of triples) to a KB (list of triples)

(defun match-lf-lists (pattern KB)
  (match-lf-lists-one-at-a-time pattern KB nil))

(defun match-lf-lists-one-at-a-time (pattern KB oldbndgs)
  (if pattern
      (multiple-value-bind (newbndgs matches)
	  (retrieve-matches (first pattern) KB nil nil)
	(if matches
	    ;; there may be multiple matches here, we need to explore each one individually eventually - right now this is just the first
	    (match-lf-lists-one-at-a-time (subst-mappings (cdr pattern) (car newbndgs)) KB (add-to-bindings (car newbndgs) oldbndgs))
	    nil))
      oldbndgs))

(defun retrieve-matches (pat KB matches bndgs)
  (if KB
      (let ((newbndgs (parser::match-with-subtyping pat (first KB))))
	(if newbndgs 
	    (retrieve-matches pat (cdr KB) (cons (first KB) matches) (cons newbndgs bndgs))
	    (retrieve-matches pat (cdr KB) matches bndgs)))
      (values bndgs matches)))

(defun add-to-bindings (new old)
  (if old
      (if (equal new '((nil nil)))
	  old
	  (append new old))
      new))
