;; Code to match and extract information from LFs - reusing the MatchEngine code also used for speech act interpretation

(in-package :IM)

(defvar *extraction-rules* nil) ;; the ids for the extraction rules used in this application
(defvar *term-extraction-rules* nil) ;; the rule ids for terms in this application
(defvar *post-extraction-rules* nil)  ;; rule ids for post processing
(defvar *substitute-terms-in-extraction* nil) ;; if set to T, the results of TERM extraction are used in the next extraction phase
(defvar *substitute-types-in-pros* nil)

(defun do-extractions (rec rule-groups)
  (trace-msg 1 "~%Performing Extractions with ~S" rule-groups)
  (let* ((lfs (if (utt-record-speechactID rec)
		  (remove-unused-context-with-root (utt-record-speechactID rec) (make-lf-list rec))
		  (make-lf-list rec)))
	 (extractions
	  (extract-events-from-lfs (substitute-pro-types lfs rec) (utt-record-uttnum rec) rule-groups)))
    
    (setf (utt-record-extractions rec) (append (utt-record-extractions rec) extractions))
    (values extractions lfs)))

(defun substitute-pro-types (lfs rec)
  "this substitues the type of the referent into the LF for the pro in order to faciliate better matching"
  (if *substitute-types-in-pros*
      (mapcar #'substitute-pro-type lfs)
      lfs))

(defun substitute-pro-type (lf)
  (if (member (car lf) '(ont::pro ont::pro-set ont::impro))
      (let* ((ref (or (find-arg-in-act lf :ref) (find-arg-in-act lf :coref)))
	     (reflf (if ref (getLFform ref)))
	     (start (find-arg-in-act lf :start))
	     (end (find-arg-in-act lf :end))
	     )
	(if reflf
	    (list* (car lf) (cadr lf) (third reflf) 
		   :start start :end end :coref ref 
		   (remove-arg (remove-arg (remove-arg (cdddr reflf) :start) :end) :coref))
	    lf))
      lf))

(defun continue-extractions (prior-extractions lfs rec rule-groups)
  (trace-msg 1 "~%Performing Extractions with ~S" rule-groups)
  (let* ((new-lfs (if *substitute-terms-in-extraction* 
		      (let ((newlfs (replace-lfs-with-terms prior-extractions lfs nil)))
			(trace-msg 2 "~%Replacing LFS with extracted terms: ~S resulting is ~S" prior-extractions newlfs)
			newlfs)
		      lfs))
	 (extractions (extract-events-from-lfs new-lfs (utt-record-uttnum rec) rule-groups)))
  (setf (utt-record-extractions rec) (append (utt-record-extractions rec) extractions))
  (values extractions new-lfs)))

(defun replace-lfs-with-terms (terms lfs output)
  "Replace LFs with extractions of the same name, and also passes along new extractions that don't correspond to any LF"
  (let* ((term-list (append-lists terms))
	 (term-vars (mapcar #'(lambda (x) (second x)) term-list))
	 (lf-vars (mapcar #'(lambda (x) (second x)) lfs))
	 (newterm-vars (set-difference term-vars lf-vars))
	 (newterms (if newterm-vars (remove-if-not #'(lambda (x) (member (car x) newterm-vars)) term-list))))
    (append (replace-lfs-with-terms+ terms lfs output) newterms)))

(defun append-lists (list)
  "reduces a list of lists to a list"
  (when list
    (append (car list)
	    (append-lists (cdr list)))))

(defun replace-lfs-with-terms+ (terms lfs output)
  "Replaces the LFs with the terms that have the same variable name. Note each term result is a list of new
     extracted formulas, but the variable we are looking for should be the one in the first extract term"
  (if lfs
    (let* ((first-lf-var (cadr (car lfs)))
	  (term-with-same-var (find-if #'(lambda (x) (eq (second (car x)) first-lf-var)) terms)))
      (replace-lfs-with-terms+ terms (cdr lfs) (if term-with-same-var 
						  (append (expand-with-unused-features term-with-same-var (car lfs)) output)
						  (cons (car lfs) output))))
    (reverse output)))

(defun expand-with-unused-features (term lf)
  "this add any additional features in the LF that were not used in the matching! - how do we do this?"  ;;  FIX ME
  term)

(defun extract-events-from-lfs (lfs uttnum rule-groups)
   (trace-msg 3 "Starting extraction on ~S with rules ~S" lfs rule-groups)
   (let* ((tmalfs (if im::*compute-force-from-tmas* (im::interpret-tmas lfs)
		    lfs))
	 (newlfs tmalfs)) ;; (refine-types-based-on-coref tmalfs)))
     (let ((results 
	    (compute-entailments newlfs rule-groups)))
	(trace-msg 3 "~%Extraction results are ~S" results)
	results)))

(defun refine-types-based-on-coref (lfs)
  "This looks for REFERENTIAL-SEMS with COREF information and replace the type from the
      discourse record"
  (multiple-value-bind (corefs others)
      (split-list #'(lambda (x) (member :coref x)) lfs)
    (instantiate-coref corefs others)))

(defun instantiate-coref (corefs others)
  (if corefs
    (let ((first (car corefs)))
      (let* ((anteid (find-arg-in-act first :coref))
	     (antelf (getLFform anteid)))
	(if antelf
	    (instantiate-coref (cdr corefs) (subst anteid (cadr first) 
						   (cons first others)))
	    (instantiate-coref (cdr corefs) (cons first others)))))
    others))

(defun abbreviate-lf (lf)

  (list (car lf) (cadr lf) (caddr lf)))

(defun debug-extraction (ruleid lfs)
  (im::debug-rule ruleid lfs *extraction-rules*))
