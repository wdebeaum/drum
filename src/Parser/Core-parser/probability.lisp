;;  PROBABILITY.LISP
;;
;;  This provides the interface to compute probabilities of arcs. It may be rewritten to
;;  implement different models as long as the first two functions are defined. Both these
;;    function should return two values: the first being the new probability value, and
;;    the second being auxiliary information you'd like to store for later computations.
;;    This information will be stored in the prob-aux field of the ARC and ENTRY.


(in-package :parser)

;; PROBABILITY COMPUTATION API

;; The following two functions should return two values: the first is the probability for
;;   the new arc/entry, and the second is any auxiliary data you want to save to it can be
;;   retrieved in a later calculation. To get the saved information from an arc or entry,
;;   use the functions (arc-prob-aux arc) and (entry-prob-aux entry)
;;   In the generic verisons below, we don't use auxiliary information. I've just put in
;;   aomts there just to show how the information would be returned.

;; FIRST FUNCTION in API

;;    (compute-new-arc-prob rule entry start bndgs) -compute the probability of an entry
;;               constructed from a RULE with the first contituent on the RHS filled by ENTRY.
;;               it differs from the second function in that no arc has been built yet. it will
;;               only be built when this action gets to the top of the agenda.
;; This default model uses the predefined probabilities assigned in the grammar
;;   Note:: ENTRY may be nil if there is no entry matching this new arc yet


(defun compute-new-arc-prob (rule entry start bndgs)
  (declare (ignore start bndgs))
  (values
   (if entry
       (normalize 
	(* (rule-prob rule) (entry-prob entry)
	  (compute-barrier-penalty (constit-cat (rule-lhs rule)) start entry T (rule-id rule) start)))
     (rule-prob rule))
   nil))

(defun get-next-cat (cs)
  "Finds the first constituent in the list, if there is any!"
  (when cs
    (let ((c (car cs)))
      (cond ((and (constit-p c) (not (check-for-defined-predicate (constit-cat c))))2221
	     (if (var-p  (constit-cat c))
		 (car (var-values (constit-cat c)))
		 (constit-cat c)))
	    ((get-cat-from-var c))
	    (t (get-next-cat (cdr cs)))))))

(defun get-cat-from-var (v)
  (and (var-p v)
       (let ((vl (var-values v)))
	 (if (constit-p vl) 
	     (constit-cat vl)
	     (if (Consp v) (car v1))))))
		       
;; SECOND FUNCTION in API

;;  THIS is the maion function for computing probabilityes.
;;  (compute-extended-arc-prob (arc entry start bndgs) - computes probability of a new arc constructed by
;;      extending an existing ARC with the specified ENTRY. The entry might complete the arc resulting in
;;      a new constituent. You can check this by looking at the ARC-POST - if there's one constituent left,
;;      the ENTRY wil complete the arc.
;;   ENTRY is always specified, and cannot be NIL.
;;  Note: start position can be extracted from both ARC and the ENTRY.

;;(defvar *barrier-penalty* nil)
;;(defvar *barrier-violation-count* 0)

(defvar *no-full-crossing* nil)

(defun compute-extended-arc-prob (arc entry bndgs)
  (declare (ignore bndgs))
  (normalize 
   (* (arc-prob arc) (entry-prob entry)
     (compute-barrier-penalty (constit-cat (arc-mother arc)) (arc-end arc) entry (get-next-cat (cdr (arc-post arc))) (arc-rule-id arc) (arc-start arc)))))

(defun compute-barrier-penalty (mothercat posn entry next-cat rule-id start)
    ;; if we have barrier processing on, we check that here: we are trying to build a MOTHERCAT by extending an arc at POSN with ENTRY
     ;; with a constituent NEXT-CAT needed afterwards
    (if (and (barrier-penalty *chart*) (barriers-computed *chart*))
	(let* ((barrier (aref (barriers *chart*) posn))
	       (bracketing (barrier-bracketing (aref (barriers *chart*) start)))
	       (starts (barrier-starts barrier))
	       )
	  ;;(format t "~%Checking ~S,~S with barrier ~S" posn (entry-end entry) barrier)
	  (if barrier
	      (if (and (partial-crossing-test bracketing start (entry-end entry))   ;; check for bracket crossing violation wrt beginning for constituent that is being built
		       (or (not starts)
			   (arc-start-test (get-entry-cat entry) (entry-first-cat entry) starts  posn (entry-end entry))) ;;; check if proposed C could start predicted constit at this posn 
		       (or *no-full-crossing*
			   next-cat
			   (full-crossing-test start (entry-end entry))))   ;; if arc is completed, check crossing wrt the end
		  1
		  (get-barrier-penalty rule-id start posn entry barrier))
	      1))
	1
    ))

(defun arc-start-test (entry-cat leftmostcats starts arc-start entry-end)  ;; I'm deleting this as its overkill and prone to problems
  T)
 #|| "is it possible for the an entry with category ENTRY-CAT, ending at ENTRY-END, and with
        leftmost subconstituents LEFTMOSTCATS
        to start an arc in the final tree containing the predicted constits in STARTS"
  ;;(and (crossing-test arc-start entry-end)
       (every #'(lambda (start)
		  (let ((predicted-cats (car start))
			(startingcohorts (cadr start))
			(prediczaffted-end (caddr start)))
		    (or ;; entry could be the predicted cat
		     (and (member entry-cat predicted-cats) (=  entry-end predicted-end))
		     ;; entry could be above predicted cat in the tree
		     (and  (intersection predicted-cats Leftmostcats) (>=  entry-end (caddr start)))
		     ;; entry could be below predicted cat in the tree
		     (and (member entry-cat startingcohorts) (<= entry-end (caddr start))))))
	      starts))||#

(defun partial-crossing-test (bracket start end)
  "checks for boundary crossing violations -- we are building an arc, so END is not the final end -
     but we can check for a violation so far"
  (if (and bracket (< (caddr bracket) end))
      (progn (trace-msg 2 "~%Crossing violation (incremental)")
	     nil)
      t))

(defun full-crossing-test (start end)
  "checks for boundary crossing violations -- we have completed an arc, so END is finally known - we have already
     checked brackets at the start"
  ;;(format t "~% Checking bracket ~S, ~S with bracket ~S" start end (aref (barriers *chart*) end))
  (let* ((barrier (aref (barriers *chart*) end))
	 (bracket (if barrier (barrier-bracketing barrier))))
  (if (and bracket
	   (or (< start (cadr bracket))))
      (progn (trace-msg 2 "~%Crossing violation (on constit that would be built)")
	     nil)
	 t)))


(defun get-barrier-penalty (rule-id start posn entry barriers)
  (trace-msg 2
	  "~%Penalizing arc extension of ~S (starts at ~S) at posn ~S with ~S ending at ~S for barrier violation"
	     rule-id start posn (entry-name entry) (entry-end entry))
  
  (setf (barrier-violation-count *chart*) (+ (barrier-violation-count *chart*) 1))
  ;;(untrace)
  (barrier-penalty *chart*))

(defun get-entry-cat (e)
  (let ((cat
	 (cond ((entry-p e)
		(constit-cat (entry-constit e)))
	       ((constit-p e)
		(constit-cat e))
	       ((var-p e)
		(let ((c (var-values e)))
		  (if (constit-p c)
		      (constit-cat c)))))))
    (when (not (check-for-defined-predicate cat))
      cat)))
	
	 
;; Here's an example function that extracts out information from the HEAD constituent
;;  We have a ARC constructed from a rule, and are extending it with a new ENTRY.
;;  The head has already been seen (and so is in the list of already parsed ENTRYs in (ARC-PRE arc)
;;  or is the next consituent in RHS of the rule (arc-post arc), and hence the ENTRY is the head,
;;  or it is later in the arc-post, in which case we have no information yet about the head.

;;  Note: this could be called from COMPUTE-EXTENDED-ARC-PROB with the ARC and ENTRY, but
;;    would not work on 
(defun find-head-info (arc entry)
  (let* ((head-entry (or (find-if #'(lambda (x) (and (entry-p x) (constit-head (entry-constit x)))) (arc-pre arc))
			 (if (let ((firstpost (car (arc-post arc))))
			       (and (constit-p firstpost) (constit-head firstpost)))
			     entry)))
	 (head (if head-entry (entry-constit head-entry))))
    (if head
	(format t "~%Rule ~S with head lex ~S and cat ~S" (arc-rule-id arc) (get-value head 'w::lex)
		(constit-cat head)))
    (if head	(values (get-value head 'w::lex)
		(constit-cat head)))))
