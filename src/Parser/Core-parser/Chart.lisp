(in-package "PARSER")

;;
;; Chart.lisp
;;
;; Functions for managing the chart.
;;
;;
;; Author: James Allen
;; History:

;;;======================================================================
;;; NLP code for use with Natural Language Understanding, 2nd ed.
;;; Copyright (C) 1994 James F. Allen
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;======================================================================

;; THE CHART DATA STRUCTURE
;;
;;  Throughout, the term "arc" will be used for non-completed active arcs, and
;;    "entry" will be used for completed arcs containing a constituent.
;;
;; The chart is stored in four different variables:
;;     chart-arcs - an array that stores arcs indexed by their ending position
;;     constits-by-name chart-entries - an assoc list all the
;;       completed constituents, allows the code to access consituent
;;       by their unique name.
;;     constits-by-position - an array of the
;;       completed constituents indexed by their beginning
;;       position. This is not by the parser, but is used in analyzing the chart
;;       for finding best answers, etc.

;; gf 5 May 2006: Start refactoring of chart code

(defvar *max-utterance-length* 2000)  ;; accepts utterance up to 20 seconds (good luck parsing it!!)

(defclass chart ()
  ((numberEntries :initform 0 :accessor numberEntries)   ;; the number of entries currently on the chart
   (warned-bailing-out :initform nil :accessor warned-bailing-out) ;; the var to let you know that the warning was printed already
   (chart-arcs :initform nil :accessor chart-arcs)
   (constits-by-name :initform nil :accessor constits-by-name)
   (maxChartSize :initform *max-utterance-length* :accessor maxChartSize)
   (maxNumberEntries :initform 1500 :accessor maxNumberEntries) ;; number of chart  entries before stopping
   (constits-by-position :initform nil :accessor constits-by-position)
   (word-count :initform 0 :accessor word-count)
   (max-position-found :initform 0 :accessor max-position-found)
   (min-position-found :initform 10000 :accessor min-position-found)
   (chart-input-ends :initform '(0) :accessor chart-input-ends)
   (chart-ends-calculated :initform nil :accessor chart-ends-calculated)
   (constituent-count :initform 0 :accessor constituent-count)
   (stopflag :initform nil :accessor stopflag)
   (threshold :initform 0 :accessor threshold) ;; lower limit on probability for processing
   (end-seen :initform nil :accessor end-seen)
   (number-parses-found :initform 0 :accessor number-parses-found)
   (number-parses-desired :initform 1 :accessor number-parses-desired) ;; # interpretations to return
   (number-parses-to-find :initform 1 :accessor number-parses-to-find)  ;; # spaning parses to find before stopping
   (stopwhensolutionsfound :initform t :accessor stopwhensolutionsfound)
   (standalone :initform nil :accessor standalone)   ;; t if the parser is NOT acting on a dialog
   (agenda :initform (make-array 1000 :initial-element nil)
	   :accessor agenda)
   (lex-items :initform (make-hash-table) :accessor lex-items)
   (suppressed-lex-entries :initform nil :accessor suppressed-lex-entries)
   (top-bucket :initform 0 :accessor top-bucket)
   (skeleton :initform nil :accessor skeleton)
   (barriers  :initform nil :accessor barriers)
   (barriers-computed  :initform nil :accessor barriers-computed)
   (barrier-violation-count  :initform 0 :accessor barrier-violation-count)
   (barrier-cats  :initform nil :accessor barrier-cats)
   (barrier-penalty :initform 1 :accessor barrier-penalty)
   (flexible-semantic-matching :initform nil :accessor flexible-semantic-matching)
   )
  (:DOCUMENTATION "Class representing chart used by parser. Previously these
attributes were local variables visible only to the following functions."))
  
(defvar *chart* (make-instance 'chart)
  "Chart instance used for parsing. This is declared as a special
variable (rather than a local) to allow separate threads to manage
separate instances of the chart/parser.")

;; The following functions operate on the default chart instance
;; stored in *chart* (previously done using local variables)

  (defun setMaxChartSize (n)
    (setq *max-utterance-length* n)
    (setf (maxChartSize *chart*) n))
  
  (defun getMaxChartSize nil
    (maxChartSize *chart*))

  (defun getmaxnumberentries nil
    (maxNumberEntries *chart*))
  
  (defun setmaxnumberentries (n)
   (setf (maxNumberEntries *chart*) n))

       
  ;;  This initializes structures of the appropriate size for a given sentence.
       
  (defun make-chart (size)
    (if (null size) (setq size (maxChartSize *chart*)))
    (setf (numberEntries *chart*) 0)
    (setf (warned-bailing-out *chart*) nil)
    (setf (chart-arcs *chart*) (make-array (list (1+ size)) :initial-element nil))
    (setf (constits-by-position *chart*) (make-array (list (1+ size)) :initial-element nil))
    (setf (constits-by-name *chart*) nil)
    (setf (lex-items *chart*) (make-hash-table))
    )

  ;;  Functions for remembering and reinstantng prior charts
  
  (defun chart-snapshot nil
    (list 'CHART (chart-arcs *chart*) (constits-by-position *chart*) (constits-by-name *chart*)
	  (word-count *chart*) (get-max-position)))
  
  (defun instantiate-chart (chart)
    (if (and (listp chart) (eq (car chart) 'chart) (eql (list-length chart) 6))
	(let nil
	  (setf (chart-arcs *chart*) (second chart))
	  (setf (constits-by-position *chart*) (third chart))
	  (setf (constits-by-name *chart*) (fourth chart))
	  (set-word-count (fifth chart))
	  (reset-max-position (sixth chart)))
      (parser-warn "Illegal chart initialization")))
    
  (defun get-chart-arcs nil (chart-arcs *chart*))
  (defun get-constits-by-position nil (constits-by-position *chart*))
  (defun get-constits-by-name nil (constits-by-name *chart*))

(defun get-constits-from-chart (start end &key constits)
  (if (null end)
    (setq end (get-max-position)))
  (cond
   ((null start)
    ;;  if start is not defined, neither is end.. We get constit names directly.
    (mapcar #'cadr (get-constits-by-name)))
   ((and (numberp start) (numberp end) (<= start end))
    (remove-if-not #'(lambda (x) (or (null constits) (member (constit-cat (entry-constit x)) constits)))
		   (gather-names start end))
    )
   (t (Format t "Bad positions in chart specified")
      nil)))

(defun get-constits-from-chart-span (start end &key constits)
  (when (null end)
    (setq end (get-max-position)))
  (when (null start)
    (setq start (get-min-position)))
  (if (and (numberp start) (numberp end) (<= start end))
      (remove-if-not #'(lambda (x) (and (eql (entry-end x) end)
					(or (null constits) (member (constit-cat (entry-constit x)) constits))))
		     (aref (get-constits-by-position) start))
    (Format t "Bad positions in chart specified"))
  )


;;  GET ENTIRE SUBTREE UNDER A CONSTITUENT
;;  we traverse the subconstituents apply variable bindings as we go

(defun get-tree (entry bindings)
  (Get-t entry bindings nil))

(defun get-t (entry bindings vars-seen)
  (let* ((subconstitnames (getsubconstitnames 1 (entry-constit entry)))
         (subentries (mapcar #'get-entry-by-name subconstitnames))
         (subconstits (mapcar #'entry-constit subentries))
         (bndgs (merge-lists
                 (cons bindings (mapcar #'robust-constit-match
                                        (entry-rhs entry) subconstits)))))
    (multiple-value-bind (subs new-vars-seen)
	(subst-in-list subentries bndgs vars-seen)
      
      (cons
       (get-entry-with-name entry)
       (mapcar #'(lambda (e) (get-t e bndgs new-vars-seen))
	       subs)))))

(defun robust-constit-match (rhs c)
  "This generalizes the constit-match function so that it can function when there are multiple entires for the same slot,
      as often happens in LFs with several MOD functions"
  (or (constit-match rhs c)
      (progn
	(trace-msg 2 "~%constit-match failed on ~S and ~S" rhs c)
	nil)))

(defun getsubconstitnames (n constit)
  (let ((sub (get-value constit n)))
    (if sub (cons sub (getsubconstitnames (1+ n) constit))
        nil)))

(defun get-subnodes (entryname)
  "Like get-tree but returns the names, not the entries"
  (let ((*vars-seen* nil))
    (Get-subnodes+ (get-entry-by-name entryname))))

(defun get-subnodes+ (entry)
  "CALL ONLY FROM GET-SUBNODES"
  (when entry
    (let* ((subconstitnames (getsubconstitnames 1 (entry-constit entry)))
	   (subsubnames (mapcar #'get-subnodes+ (mapcar #'get-entry-by-name subconstitnames))))
      (append subconstitnames (flatten subsubnames)))))

;;  RESTARTING PARSER WITH SOME CONSTITUENTS PRE-LOADED
(defun restart-parse-with-constits (cs)
  (if cs
      (let ((subentries (mapcar #'get-entry-by-name
			 (remove-duplicates (flatten (mapcar #'get-subnodes cs)))))
	    (topentries (mapcar #'get-entry-by-name cs)))
	(parse '(start-sentence))
	(mapcar #'reintroduce-entry topentries)
	(mapcar #'put-in-chart subentries)  ;; the subtrees go back in parse but do not retrigger any arcs
	(compute-prefer-msgs topentries nil))
      (progn
	(parse '(start-sentence))
	nil)))

(defun reintroduce-entry (e)
  ;;(add-entry-to-chart-continued e)
  (when (put-in-chart e)
    (setf (constituent-count *chart*) (+ (constituent-count *chart*) 1))
    (Make-New-BU-Active-Arcs e (entry-name e))
    (Chart-Extend e (entry-name e)))
  (update-max-position-found (entry-end e))
  (update-min-position-found (entry-start e)))

(defun compute-prefer-msgs (cs msgs)
  "this takes a list of constituents and returns a list of prefer messages for those constituents"
  (if (null cs) msgs
      (let* ((c (car cs))
	    (cat (constit-cat (entry-constit c)))
	    (start (entry-start c))
	    (end (entry-end c)))
	(if (not-in-msgs cat start end msgs)
	    (compute-prefer-msgs (cdr cs) (cons (list 'prefer "" :frame (list start end) :penn-cats (list cat)) msgs))
	    (compute-prefer-msgs (cdr cs) msgs)))))

(defun not-in-msgs (cat start end msgs)
  (if (null msgs)
      t
      (let ((msg (car msgs)))
	(if (and (eq start (car (fourth msg)))
		 (eq end (cadr (fourth msg)))
		 (eq cat (car (sixth msg))))
	    nil
	    (not-in-msgs cat start end (cdr msgs))))))

       
  ;;  MAINTAINING THE ACTIVE ARCS ON THE CHART
       
  ;;  Adding an arc to the chart 
  (defun add-arc-to-chart (arc)
    (let ((e (arc-end arc)))
      ;;(up-arc-count (arc-rule-id arc))
      (if (< e (maxChartSize *chart*))
        (setf (aref (chart-arcs *chart*) e) (cons arc (aref (chart-arcs *chart*) e))))))
       
  ;; Retrieving all arcs ending at a specified position p
       
  (defun get-arcs (p)
    (if (< p (maxChartSize *chart*))
	(aref (chart-arcs *chart*) p)))
       
  ;;   PUT-IN-CHART  - adds an entry e identified by symbol name into the
  ;;     chart data structures, unless an identical one is already there
  ;;     or if the constituent is non-empty and has a gap feature of
  ;;     same category (e.g., NP/NP)
  ;;   Returns t is constituent is new 
     
     (defun put-in-chart (newentry)
       (setf (numberEntries *chart*) (+ (numberEntries *chart*) 1))
       ;;(up-constit-count (entry-rule-id newentry))
       
       (if (and *beam-pruning-on*
		(= (mod (numberEntries *chart*) *pruning-frequency*) 0))
	   (prune-agenda *beam-width*))
	   
       (if (> (numberEntries *chart*) (maxNumberEntries *chart*))
	   (when (not (warned-bailing-out *chart*)) 
	     (setf (warned-bailing-out *chart*) t)
      	     (format t "~%Parser bailing out~%")
	     nil
	     )
         (let* ((start (entry-start newentry))
                (name (entry-name newentry)))
           (when (filter-constit start (entry-end newentry) (entry-constit newentry) newentry)
             (setf (constits-by-name *chart*) (cons (list name newentry) (constits-by-name *chart*)))
	     (if (< start (maxChartSize *chart*))
               (setf (aref (constits-by-position *chart*) start)
                     (cons newentry (aref (constits-by-position *chart*) start))))
               t))))
       
     ;;   get-entry-by-name retrieves a constituent given its unique identifier
     
     (defun get-entry-by-name (name)
       (cadr (assoc name (constits-by-name *chart*))))
     
     ;;    get-entries-by-position returns all entries with indicate CAT
     ;;      that start at pos, and SKIP entries at pos
     
     (defun get-entries-by-position (cat pos)
       (when (< pos *max-utterance-length*)
	 (if cat 
	     (remove-if-not #'(lambda (e)
			  (let ((c (entry-constit e)))
			    (or (match-vals 'CAT (constit-cat c) cat)
				(eq (constit-cat c) 'unknown)
				(eq (get-value c 'w::SKIP) '+))))
			    (aref (constits-by-position *chart*) pos))
         ;; if cat not specified, return them all
	   (aref (constits-by-position *chart*) pos))))

     ;;   REMOVING CHART ENTRIES FOR BACKING UP
     ;;   This is only used to support backspacing in the online parser
     
     (defun ClearChartAfter (n)
       ;;    clear the arcs and entries after position n
       (do ((i (+ n 1) (+ i 1)))
	   ((= i (maxChartSize *chart*)))
	 (setf (aref (chart-arcs *chart*) i) nil)
	 (setf (aref (constits-by-position *chart*) i) nil))
       ;;   clear out constituents that end at n or after
       (setf (constits-by-name *chart*)
	 (remove-if #'(lambda (pair)
			(> (entry-end (cadr pair)) n))
		    (constits-by-name *chart*)))
       (dotimes (i (+ n 1))
	 (setf (aref (constits-by-position *chart*) i)
	   (remove-if #'(lambda (c)
			  (> (entry-end c) n))
		      (aref (constits-by-position *chart*) i))))
       )
       
;; end of functions that operate on *chart*

;; FILTER-CONSTIT - the constituent filter. Returns t if the constituent should
;;   be added to the chart. Currently this checks three things:
;;      - whether an identical constituent is already on the chart
;;      - whether the constituent has an illegal GAP feature, i.e., 
;;           a non-empty constit of cat C with a gap of cat C
;;      - whether the start position exceeds the chart size


(defun filter-constit (start end constit newentry)
  (let* ((cat (constit-cat constit))
         (feats (constit-feats constit))
         (existing-entries (get-entries-by-position cat start))
         (gapval (get-value constit 'w::gap)))
    (cond 
     ;;   check for duplicate entry
     ((some #'(lambda (e)
		(and (eql (entry-end e) end)
		     (identical-feats (constit-feats (entry-constit e))
				      feats)))
	    existing-entries)
       (trace-msg 2 "~% Not adding duplicate entry ~S" newentry)	
       nil)
      ;;  check for non-empty constit of form X/X   -- counter example "what did you thwart the passage of"
      ;;((and (constit-p gapval) (eq (constit-cat gapval) cat)
       ;;       (not (equal (get-value constit 'EMPTY) '+)))
      ;; (trace-msg 2 "~% Not adding X/X entry ~S" newentry)
      ;; nil)
      ((< start (getMaxChartSize)) t)
      (t (parser-warn
          "****WARNING: INPUT IS TOO LARGE FOR CURRENT CHART SIZE
 Current size is ~s. Use SetMaxChartSize to increase default chart size" (getMaxChartSize))
	 (suspendParse)
	 nil))))


;;  This returns true if the features are identical up to variable renaming

(defun identical-feats (fl1 fl2)
  (if (eql (list-length fl1) (list-length fl2)) 
       (let ((bndgs (fconstit-match fl1 fl2)))
         ;;  check each binding. Value must be an unconstrained variable
         (if bndgs
           (every #'(lambda (pair)
                    (or (equal pair '(nil nil))
                        (and (var-p (cadr pair))
			     (null (var-values (cadr pair))))))
		  bndgs)))))

;;=======================================================================
;;   BEAM search pruning
;;   This prunes the agenda leaving a maximum of BEAM-WIDTH items between any pair of points

(defvar *beam-pruning-on* nil)
(defvar *pruning-frequency* 500)  ;; we prune the agenda every N times
(defvar *beam-width* 10)            ;; we take only the N best constituents between any to locations
(defvar *count-table* )

(defun generate-constit-counts nil
  (let ((count-table (make-array (list (1+ (maxchartsize *chart*))) :initial-element nil)))  ;; count tabel is a sparse array (end posn are assoc lists)
    (dotimes (i *number-of-buckets-for-agenda*)
	     (count-in-bucket (aref (agenda *chart*) i) count-table))
    (show-table count-table)))

(defun prune-agenda (beam-width)
  (format t "~%Pruning agenda with beam width ~S" beam-width)
  (let ((count-table (make-array (list (1+ (maxchartsize *chart*))) :initial-element nil)))
    (do ((i *number-of-buckets-for-agenda* (- i 1)))
	((= i 0))
      (if (> (list-length (aref (agenda *chart*) i)) 0)
	  (trace-msg 1 "~%Pruning bucket ~S with ~S constits" i (list-length (aref (agenda *chart*) i))))
      (when (> (list-length (aref (agenda *chart*) i)) 4000)
	(setf (aref (agenda *chart*) i) (subseq  (aref (agenda *chart*) i) 0 3999)))
      (setf (aref (agenda *chart*) i)
	    (prune-bucket (aref (agenda *chart*) i) beam-width count-table)))
    ))

(defun prune-bucket (bucket beam-width counts)
  (when bucket
    ;;(if (eq (agenda-item-type (car bucket)) 'entry)
	(let ((c (add-to-count (agenda-item-start (car bucket)) (agenda-item-end (car bucket)) counts)))
	  (if (> c beam-width)
	      (progn 
		(if (eq (agenda-item-type (car bucket)) 'entry)
		    (trace-msg 1 "~%Pruning constit ~S from ~S to ~S with score ~S"  
			       (constit-cat (ENTRY-CONSTIT (agenda-item-entry (car bucket))))
			       (agenda-item-start (car bucket)) (agenda-item-end (car bucket)) (agenda-item-score (car bucket))))
		(prune-bucket (cdr bucket) beam-width counts))
	      (reuse-cons  (car bucket) (prune-bucket (cdr bucket) beam-width counts) bucket)))))
;;	(reuse-cons (car bucket) (prune-bucket (cdr bucket) beam-width counts) bucket))))

(defun show-table (table)
  (dotimes (i (1+ (maxchartsize *chart*)))
    (dotimes (j (1+ (maxchartsize *chart*)))
      (if (> (or (cadr (assoc j (aref table i))) 0) 0)
	  (format t "~%count from ~S to ~S: ~S" i j (cadr (assoc j (aref table i))))))))

(defun count-in-bucket (bucket count-table)
  ;;(format t "~%  Next Bucket is ~S" bucket)
  (mapcar #'(lambda (x) (add-to-count (agenda-item-start x) (agenda-item-end x) count-table))
	  bucket))

(defun add-to-count (start end count-table)
  (let ((val (1+ (or (cadr (assoc end (aref count-table start))) 0))))
    (setf (aref count-table start) (push (list end val)  (aref count-table start)))
    val))
	  

;;  BUILD-ENTRY - this constructs an entry given a constit, start, end and rhs

;; Definition moved to structures.lisp for easier compilation.
;; (defstruct entry
;;  constit start end rhs name rule-id prob)

(defun build-entry (constit start end rhs rule-id prob prob-aux first-cat)
  ;;(format t "~%Building entry from rule ~S from ~S to ~S" rule-id start end)
  (let ((skeleton (if  *semantic-skeleton-scoring-enabled*
		       (compute-skeleton constit))))
    (make-entry :constit constit
		:start start :end end :rhs rhs 
		:name  (gen-reusable-symbol (if (symbolp (constit-cat constit)) (constit-cat constit)))
		:rule-id rule-id 
		:prob (adjust-prob-based-on-skeleton-score prob skeleton)
		:prob-aux prob-aux 
		:first-cat first-cat
		:size (if (and start end)
			  (- end start)
			  (add-up-sizes constit start end)) ;; I think this is obsolete
		:skeleton skeleton)
  ))

(defun add-up-sizes (constit start end)
  "add up the sizes of the cobconstituents, or get the length of the LEX symbol is no children"
  (if (get-value constit 1)
      (sum-up-subs constit 1 start end)
      (let ((w (get-fvalue (constit-feats constit) 'w::lex)))
	(if (consp w) (list-length w) 1))))

(defvar *semantic-skeleton-scoring-enabled* nil)
(defvar *semantic-skeleton-map* nil)
(defvar *var-type-map* nil)
(defvar *semantic-skeleton-score-factor* .1)
(defvar *essential-roles* '(ont::agent ont::agent1 ont::affected ont::affected1 ont::neutral ont::neutral1 ont::formal ont::result ont::affected-result ont::of ont::val ont::figure ont::ground ont::experiencer ont::source ont::transient-result ont::orientation))

;; set this variable to a set of scored skeleton debugging
(defvar *debug-skeleton-info* 
  nil)

;; set this to T if you want the skeletons generated for a sentence
(defvar *generate-skeletons* nil)

(defun adjust-prob-based-on-skeleton-score (prob skel)
  (if (null skel)
      prob
      (let ((score (cadr skel)))
	(if (not (numberp score)) (setq score 1))
	(trace-msg 3 "~%adjusting PROB by ~S" score)
	(* prob score))))

(defun lookup-ont-type-from-skeleton-map (id)
  (cond ((eq id '<var>)
	 id)
	((symbolp id)
	 (let ((entry (assoc id *var-type-map*)))
	   (values (cadr entry) (caddr entry))))
	((constit-p id)  ;; this is an IMPRO, find type directly
	 (get-simple-type (get-value id 'w::class)))))

(defun get-simple-type (x)
  (let ((type (if (consp x) (cadr x) x)))
    (if (symbolp type)
	type 
	'bad-type)))

(defun compute-skeleton (constit)
  (when (member (constit-cat constit) '(w::s w::vp w::np w::adjp w::advbl))
      (let* ((lf (get-fvalue (constit-feats constit) 'w::lf))
	     (complete-class (get-value lf 'w::class))
	     (class (if (consp complete-class) (second complete-class) complete-class))
	     (xxx (if (member class '(ont::and ont::sequence))
		      (trace-msg 2 "~%~% FOUND SEQUENCE from constit ~S" constit)))
	     (var (get-value lf 'w::var))
	     (constraint (if lf (get-value lf 'w::constraint)))
	     (role-pairs (if (and constraint (not (eq constraint '-))
				  (not (var-p constraint)))
			     (constit-feats constraint)))
	     ;; find all essential roles, convert unbound values to constant <VAR>
	     (found-bound-role nil)
	     (roles (mapcar #'(lambda (x)
				   (if (var-p (cadr x)) 
				       (list (car x) '<var>)
				       (progn (setq found-bound-role t)
					      x)))
			       (remove-if-not #'(lambda (x) (member (car x) *essential-roles*))
				   role-pairs)))
	      ;;(xxy (format t "~% cleaned up pairs are ~S" roles))
	     (skel (build-semantic-skeleton var complete-class (if found-bound-role roles) ;; only pass in roles if there is at least one bound value
					    (constit-cat constit))))
	     
	     ;;(if roles (format t "~% sleeton map is ~S" *semantic-skeleton-map*))
	     skel)))
      
(defun build-semantic-skeleton (id class roles syncat)
  ;; this builds the skeleton and adds to skeleton-map and the var-map if necessary"
  ;;(format t "~% Building skeleton for ~S constituent ~S with roles ~S " syncat id roles)
  (let ((word (if (consp class) (caddr class))))
    (setq class (if (consp class) (cadr class) class))
  (if (not (assoc id *var-type-map*))
      (progn (push (list id class word) *var-type-map*)
	     (trace-msg 3 "~%pushing ~S onto var-type-match for ~S" class id)))
  (let* ((skeleton (list* class (unpack-roles roles)))
	 (cached-skeleton (assoc skeleton *semantic-skeleton-map* :test #'equal)))
    ;; the following is for debugging - it provides a quick way to generate a chache from sentences
    (if (and *generate-skeletons* (not cached-skeleton) (> (list-length skeleton) 1))
	(push (list skeleton 1) *semantic-skeleton-map*))
    ;;(if cached-skeleton (format t "~%found cached info: ~S:" cached-skeleton))
    ;; if no cache, get the new value and record it in the cache
    (or cached-skeleton
	;; a single atom with no roles gets a score of 1
	(if (and (consp skeleton) (eq (list-length skeleton) 1))
	    (list skeleton 1))
	(let ((res (evaluate-skeleton skeleton syncat)))
	(push res *semantic-skeleton-map*)
	res)
	))))
    

(defun evaluate-skeleton (skel syncat)
  (let* ((reply (send-and-wait `(REQUEST :content (EVALUATE-SKELETON ,skel :syncat ,syncat))))
	 (score (car (or (find-arg reply :score) (find-arg-in-act reply :score)))))
    (list skel (if (numberp score)
		   (convert-to-adjustment-factor score)
		   1))))

(defvar *max-semantic-skeleton-factor* 1.05)
(defvar *min-semantic-skeleton-factor* .95)

(defun convert-to-adjustment-factor (log)
  (if (numberp log) (max (min log *max-semantic-skeleton-factor*) *min-semantic-skeleton-factor*)
      1))
#||  "This takes a log linear prob and converts to a range of adjustment scores"
  (if (numberp log)
      (let ((factor (-  *max-semantic-skeleton-factor* (* log log) )))
	(max factor *min-semantic-skeleton-factor*)
	   )
      1))||#


(defun unpack-roles (roles)
  (when roles
    (let ((rolename (keywordify (caar roles))))
      (multiple-value-bind
	    (type word)
	  (lookup-ont-type-from-skeleton-map (cadar roles))
	(multiple-value-bind 
	      (rest-types rest-words)
	    (unpack-roles (cdr roles))
	  (values (list* rolename
			 type
			 rest-types)
		  (list* rolename
			 word
			 rest-words)))))))

#||
(defun get-length (w start end)
  (cond ((symbolp w)
	 (list-length (coerce (symbol-name w) 'list)))
	((stringp w)
	 (list-length (coerce w 'list)))
	((numberp w)
	 (list-length (coerce (format nil "~S"  w) 'list)))
	((consp w)   ;; multi-word
	 (eval (cons '+ (mapcar #'(lambda (x) 
				    (get-length x start end))
				w))))
	(t (- end start)))
  )||#

(defun sum-up-subs (constit n start end)
  (let ((next (get-entry-by-name (get-value constit n))))
    (if next
	(+ (or (entry-size next) 1) (sum-up-subs constit (+ n 1) start end))
	0)))
       

;;=========================================================================
;; Maintaining the ACTIVE ARCS

;;  An Active arc is a 7-element list consisting of
;;    mother - the constituent being built
;;    pre - the subconstituents found so far
;;    post - the subconstituents still needed
;;    start - the starting position of the arc
;;    end - the current ending position of the arc
;;    rule-id - the rule used in the grammar to introduce the arc
;;    prob -  the probability score

;; Definition moved to structures.lisp for easier compilation.
;; (defstruct arc
;;  mother pre post start end rule-id prob)

#||
;;    MAKE-ACTIVE-ARC builds an active arc

(defun make-active-arc (mother pre post start end rule-id prob local-vars foot-feats)
  (make-arc :mother mother :pre pre :post post :start start :end end 
            :rule-id rule-id :prob prob :local-vars local-vars :foot-feats foot-feats))
||#

;; MAKE-ARC-FROM-RULE creates an arc from an instantiated rule 
;;        and a specified starting position. It makes copies of all unbound vars
;;        in the rule to make sure they are unique


(defun make-arc-from-rule (rule start bndgs)
  (let* ((copyrule (copy-vars-in-rule rule bndgs))
        (id (rule-id copyrule)))
    (check-for-common-vars rule copyrule)
    (multiple-value-bind
     (prob prob-aux)
     (compute-new-arc-prob rule nil start bndgs)	  
     (make-arc :mother (rule-lhs copyrule)
	       :post (rule-rhs copyrule)
	       :start start
	       :end start
	       :rule-id id
	       :prob prob
	       :prob-aux prob-aux
	       :local-vars (get-var-list)
	       :first-cat (list (constit-cat (car (rule-rhs copyrule))))))))

(defvar *empty-v1s* nil)

;; this is  code for debugging - its a sanity check on building the rules - will delete sometime ...
;; we check if any var has changed in the rules is not copied, and whether any vars that were
;;   originally empty in the rule now have a value!
(defun check-for-common-vars (r1 r2)
  "this is for debugging - r1 and r2 should have distinct variables!"
  (let* ((empty-v1s (cadr (assoc (rule-id r1) *empty-v1s*)))
	 (v1s (gather-all-vars r1))
	 (v2s (gather-all-vars r2)))
    (if (intersection v1s v2s)
	(break "Found common vars in copy of rule: ~S ~S: common is ~S" r1 r2 (intersection v1s v2s)))
    (if (not empty-v1s)
	;; first time rule has been seen
	(push (list (rule-id r1) (remove-if #'var-values v1s)) *empty-v1s*)
	(if (some #'var-values empty-v1s)
	    (break "A variable ~S in rule ~S that was originally empty is now set. Empty vars are ~S" 
		   (some #'var-values empty-v1s) (rule-id r1) empty-v1s)))
    (values)))

(defun gather-all-vars (rule)
  (union (gather-vars (rule-lhs rule))
	 (gather-vars (rule-rhs rule)))
  )

(defun gather-vars (e)
  (cond ((constit-p e)
	 (gather-vars (constit-feats e)))
	((consp e)
	 (union (gather-vars (car e)) (gather-vars (cdr e))))
	((var-p e)
	 (list e))
	))


;; EXTEND-ARC matches a constituent with the specified name
;;     against the next constituent needed for the active arc,
;;     so that a new extended arc can be created if they match. 
;;
;;  Also, this skips over unknown words and filled pauses
;;  if the *ignore-unknown-words* flag is t


(defun extend-arc (entry name arc)
  (declare (ignore name))
  ;;  check if we could skip over constituents in the rule
  (let* ((c (entry-constit entry))
	 (cat (get-value c 'cat))
	 (score 1))
    (cond   
     ;;  skip unknown words
     ((and *ignore-unknown-words* (eq cat 'unknown))
      (extend-arc-end-position arc (entry-end entry) *unknown-word-penalty* 
                               (list 'unknown (car (get-value (entry-constit entry) 'input)))))
     ;; skip constituents with SKIP feature
     ((and (eq (get-value c 'w::SKIP) '+) (setq score (compute-punc-score c (car (last (arc-pre arc))))))
      (extend-arc-end-position arc (entry-end entry) score ;;(compute-penalty-for-skipping c) 
                               (list 'skipped entry))))
    ;; match entry  to RHS of rule in any event
    (progn
      ;;(format t "~%Ready to extend arc ~S with constit ~S" (arc-rule-id arc) (entry-name entry))
      (multiple-value-bind (bndgs prob)
	  (match-next-pattern-in-rhs (arc-post arc) c)
	;;(format t "~%Done, match prob is ~S" prob)
	(When bndgs
	  (let ((arcprob (if (and prob (< prob 1))
			     (let ((newprob (* (compute-extended-arc-prob arc entry bndgs) prob)))
			       (trace-msg 1 "~%extend: SEM unify failed - penalizing arc ~S, from ~S to ~S by ~S to ~S" (arc-rule-id arc) 
					  (arc-start arc) (entry-end entry) prob newprob)
			       newprob)
			     (compute-extended-arc-prob arc entry bndgs))))
	  (Add-to-agenda (make-agenda-item :type 'ARC
					   :prob arcprob
					   :entry entry 
					   :start (arc-start arc)
					   :end (entry-end entry)
					   :id (arc-rule-id arc)
					   :arc arc
					   :bndgs bndgs))
	)))
    )))


;; This is a table of probabilities of punctuation immediately following a constituent, computed
;;   from the Treebank corpus.
;; Note this function is only needed when the punctuation is NOT captured explicitly in some rule.
;; returns NIL if we can't skip the constituent
(defun compute-punc-score (c prevc)
  (if (constit-p prevc)
      (if (eq (constit-cat c) 'W::pause) ;; skip filled pauses
	  .99
	  ;; otherwise we have punctuation
	  .95 ))) ;;; as we have incorporated better handling of punctuation, we are penalizing skipping more
#||	     (case (get-value c 'w::lex)
	       (W::PUNC-COMMA
		(case (constit-cat prevc) 
		  ((W::DATE W::VALUE) .99)
		  (W::NP .98)  ;; 44% or commas in Treebank follow NPs (this is .01 less than a rule explicitly matching a comma)
		  ((W::S W::SSEQ) .97)   ;; 20%  in Treebank, note we have rules explicitly expecting 
		  (W::ADVBL .97)  ;; 19%
		  (W::VP .96)  ;; 5%
		  (W::ADJP .95) ;; 1%
		  ((W::N1 W::UTT W::CP W::VP- W::V W::NPSEQ) .9)
		  ))
	       ((W::PUNC-COLON W::semi-colon)
		(case (constit-cat prevc)
		  (W::NP .98) ;; 50%
		  (W::S  .97) ;; 41% semi 15% colon
		  (W::VP  .96) ;; 10% colon
		  ((W::ADJP W::ADVBL W::PP) .95)
		  ))
	       (w::punc-minus  ;; i.e., a hyphen!
		(case (constit-cat prevc)
		  (W::NUMBER .98)
		  (otherwise .97)))
	       (w::punc-hashmark
		(case (constit-cat prevc)
		  (W::NP .98) ;; 80%
		  ((W::ADV W::PREP) .97) ;; 13%
		  (W::CONJ .96) ;; 1%
		  (otherwise .9)))
	       (otherwise .9)
	       )))))||#

(defun match-next-pattern-in-rhs (rhs c)
  "This checks if the next constit is a variable and handles that case if necesary"
  (let ((n (car rhs)))
    (if (and (var-p n) (constit-p (var-values n)))
	(multiple-value-bind (bndgs prob)
	    (constit-match (var-values n) c)
	  (if bndgs (values (add-to-binding-list (make-binding-list n c) bndgs) prob)))
      (constit-match n c))))

(defun compute-penalty-for-skipping (c)
  "computes penalty for skipping a consituent - currently very basic"
  (or (cadr (assoc (get-value c 'w::lex) (cdr *skip-constituent-penalty*)))
      (car *skip-constituent-penalty*)))

;;  EXTEND-ARC-WITH-CONSTIT builds a new active arc by extending an existing arc
;;   with a constituent. The constituent is added to the mother as a subconstituent
;;   feature: 1 for the first, 2 for the second, and so on. It also instantiates
;;   any variables indicated in the binding list bndgs, and any
;;   variables which should be bound as a result of skipping over null
;;   constituents (this happens when the GAP value of the null constit
;;   is a variable: null constits can't have GAP +).
;;   Note, with OPTIONAL or SEQUENCE constituents, multiple new arcs can be added.
;;   Note also that since this has just come off the agenda, we build the entries or arcs
;;     rather than placeing them back on the agenda.



(Defun extend-arc-with-constit (entry arc bndgs prob)
  "We know the ENTRY extends the arc. here we do it and generate successor arcs/constits"
  (let ((bound-arc (subst-in arc bndgs)))
    (multiple-value-bind (restList new-bndgsList newprob)
      (gen-new-RHsides (cdr (arc-post bound-arc)) 
		       bndgs prob)
      (mapcar #'(lambda (rest new-bndgs)
		(let* ((new-arc (if (equal bndgs new-bndgs) bound-arc (subst-in bound-arc new-bndgs)))
		       (mother (arc-mother new-arc))
		       (pre (arc-pre new-arc))
		       (Post (Cons (car (arc-post new-arc))
				     rest))
		       (start (arc-start new-arc))
		       (name (entry-name entry))
		       (end (entry-end  entry))
		       (id (arc-rule-id new-arc))
		       (local-vars (arc-local-vars new-arc))
		       (prob-aux nil)  ;; I've put this in as a temporary fix until we might ned to revive prob-aux again
		       (foot-feats (augment-foot-feats entry (car (arc-post new-arc)) 
						       (arc-foot-feats new-arc) new-bndgs)))
		  ;;(multiple-value-bind (prob prob-aux)
		  ;;    (compute-extended-arc-prob bound-arc entry bndgs)
		  
		    (cond 
		     ;; arc is completed, build a new constituent if possible
		     ((endp rest)
		      (multiple-value-bind (new-constit bndgs)
			  (Merge-feature-values mother (cons (list (+ (list-length pre) 1) name) foot-feats))
	              (if new-constit
	                ;;(if bndgs
			  ;; we add to chart immediately since this is the top prob.
			  (add-entry-to-chart
			   (subst-in (build-entry new-constit start end (append-if-necessary pre post) id newprob prob-aux 
						  (cons (constit-cat (entry-constit entry)) (arc-first-cat arc))) bndgs))
			   )
			)
		      )
		   ;; add a new active arc by extending the current one
		     (t
		      (trace-msg 1 "~%Extending arc ~S starting at ~S at ~S with entry ~S with prob ~S" (arc-rule-id arc) start (entry-start entry) name prob) ;;was 2
		      (Add-arc (make-arc :mother (Add-feature-value-to-copy mother (+ (list-length pre) 1) name)
					 :pre (append-if-necessary pre (list (car post)))
					 :post rest
					 :start start
					 :end end
					 :rule-id (arc-rule-id arc)
					 :prob newprob
					 :prob-aux  prob-aux 
					 :local-vars local-vars
					 :foot-feats foot-feats
					 :first-cat (arc-first-cat arc)
					 )))
		     )))
	      restList new-bndgsList)))
  )


;; Checks the foot features of the new subconstituent
;;  and records any values in the foot-feats slot of the arc
;;  If the foot feature is specified on the subconstituent in the rule, however,
;;  it is not propagated up to the mother

(defun augment-foot-feats (entry pattern foot-feats bndgs)
  (if (var-p pattern)
      (setq pattern (var-values pattern)))
  (let ((new-foot-feats foot-feats)
	(pattern-was-var (get-value pattern 1)))   ;; the only way the feature "1" is defined is if this is actually the constituent, i.e., there was a variable in the rule
    ;;(format t "~%calling FOOT FEATS: feats=~S ARG in entry=~S" foot-feats (get-value (entry-constit entry) 'W::ARG))
    (mapcar #'(lambda (ff)
		(let ((newval (get-value (entry-constit entry) ff))
		      (specified-in-pattern (get-value pattern ff))
		      (currentval (get-fvalue new-foot-feats ff)))
		  ;;(when (and NIL (null newval) (eq ff 'W::WH-VAR) currentval)
		    ;;(format t "~%       ff=~S entry val=~S pat val =~S currentval=~S  bndg=~S~% bndgs=~S"
		    ;;     ff newval specified-in-pattern currentval (subst-in currentval bndgs) bndgs))
		  (cond 
		   ((or (null newval) (eq newval '-) (eq newval currentval)) nil)
		   ;;  If foot-feat not found so far, add it
		   ((and (null currentval) (or  pattern-was-var (not specified-in-pattern)))
		    #||(format t "~%BINDING foot feature ~S: entry val=~S pat val =~S currentval=~S
                                    bndg=~S Value of ARG in pat=~S, in entry=~S" ff newval specified-in-pattern currentval (subst-in newval bndgs)
				    (get-value pattern 'w::arg) (get-value (entry-constit entry) 'w::arg))
		    (when  (eq ff 'W::WH-VAR) (format t "~%BNDGS=~S" bndgs)) ||#
		    (setq new-foot-feats (cons (list ff (subst-in newval bndgs)) new-foot-feats)))
		   ;;  If foot-feat is a variable, try to bind it
		   ((var-p currentval) 
		    (let ((bndgs (match-vals ff currentval newval)))
		      (if bndgs (setq new-foot-feats (subst-in new-foot-feats bndgs))))))))
	    (get-foot-features))
    new-foot-feats))
			  

;; this simply creates a new arc with the end position increased
;;  It is used to skip over unknown words and filled pauses
;; and lowers the probability by the indicated penalty factor.

(defun extend-arc-end-position (arc new-end-posn penalty notes)
  (add-arc (make-arc :mother (add-notes (arc-mother arc) notes)
		     :pre (arc-pre arc)
		     :post (arc-post arc)
		     :start (arc-start arc)
		     :end new-end-posn
		     :rule-id (arc-rule-id arc)
		     :prob (normalize (* (arc-prob arc) penalty))
		     :prob-aux (arc-prob-aux arc)
		     :local-vars (arc-local-vars arc)
		     :foot-feats (arc-foot-feats arc)
		     :first-cat (arc-first-cat arc)
		     )))

(defun add-notes (constit notes)
  "simply appends notes onto the value of the NOTES feature"
  (replace-feature-value constit 'notes 
			 (cons notes (get-value constit 'notes))))

(defun extract-values-from-notes (notes featname)
  "returns a list of all values recorded under the featname"
  (when notes
    (if (eq (caar notes) featname)
	(cons (cadar notes)
	      (extract-values-from-notes (cdr notes) featname))
       (extract-values-from-notes (cdr notes) featname))))
  
;; given a list of constituents, returns the list after skipping any
;; leading null constituents, i.e. elements of the list whose cat is
;; '- and whose GAP is not constrained to be '+.
;;   Also skips elements that are simply EQ to '-
;;   It also checks for the user defined lisp calls

(defun gen-new-RHsides (RHS bndgs prob)
  (multiple-value-bind (newRHS new-bndgs newprob)
      (remove-null-prefix RHS bndgs prob)
     ;; if the next constituent is optional, also generate RHSs where it is skipped
    (let ((next (get-next-rhs-constit newRHS)))
      (if (and next (constit-optional next)
	       (no-required-gap-check next))
	  ;; next constit is optional, so generate a RHS without it
	  (multiple-value-bind (RHSlist new-bndgs-list newerprob)
	      (gen-new-RHsides (cdr newRHS) new-bndgs newprob)
	    (values (cons newRHS RHSlist) (cons new-bndgs new-bndgs-list)
		    newerprob))
	(values (list newRHS) (list new-bndgs) newprob)))))


(defun get-next-rhs-constit (rhs)
  (when rhs
    (if (var-p (car rhs))
      (var-values (chase-down-var (car rhs)))
    (car rhs))))

(defun no-required-gap-check (c)
  "Checks that a consitutent in the RHS does not require a gap. If so, we can't skip"
  (match-vals nil (get-value c 'w::gap) '-))

(defun remove-null-prefix (cl bndgs oldscore)
  (if cl
      (if (not (eq (car cl) '-))
	  (multiple-value-bind 
		(new-bndgs score)
	      (match-for-skip (car cl))
	    
	    (cond ((equal new-bndgs *success*)
		   (remove-null-prefix (cdr cl) bndgs (combine-score oldscore score)))
		  (new-bndgs
		   (remove-null-prefix (subst-in (cdr cl) new-bndgs)
				       (append-if-necessary bndgs new-bndgs)
				       (combine-score oldscore score)))
		  (t
		   (values cl bndgs oldscore))))
	  (remove-null-prefix (cdr cl) bndgs oldscore))
      (values nil bndgs oldscore)))

(defun combine-score (old new)
  "just multiplying but checking for NIL values"
  (cond ((or (null old) (not (numberp old)))
	 new)
	((or (null new)(not (numberp new)))
	 old)
	(t (* new old))))
	


(defun match-for-skip (constit)
  (if (constit-p constit)
    (cond
     ;;  The empty constituent '- can be skipped anytime
     ((or (eq (constit-cat constit) '-) (var-p (constit-cat constit)))
      (constit-match constit *empty-constit*))
     ;;  check for a user defined procedure call
     ((check-for-defined-predicate (constit-cat constit))
      (let ((pred (check-for-defined-predicate (constit-cat constit)))
	    (args (constit-feats constit)))
	(trace-msg 3 "~%Calling User predicate ~S~%" (cons (constit-cat constit) args))
	(multiple-value-bind
	      (result score)
	    (apply pred (list args))
	  (trace-msg 3 "~%      --~A" (if result "SUCCEEDED" "FAILED"))
	  ;;(format t "~%called ~S on ~S score is ~S  result is ~S" (constit-cat constit) args score result)
	  (values result score))
	)))
    ;; a var bound to empty constit also should be skipped
    (if (and (var-p constit) (constit-p (var-values constit))
	     (eq (constit-cat (var-values constit)) '-))
	*success*)
      
     ))


;;   ADD-ARC  Adds a non-completed arc to the chart, and looks to extend it with gaps
;;     or entries already on the chart

(defun add-arc (arc)
  (trace-arc arc)
  (add-arc-to-chart arc)
  
  (cond 
   ((and (GapsEnabled) ;; this succeeds if it inserts a gap
  	 (insert-gap arc *default-grammar*)) t)
  ;;  check existing entries on the chart to see if they extend the arc
   (t (let* ((nextc (car (arc-post arc)))
	     (nextcat (cond ((constit-p nextc) (constit-cat nextc))
			    ((and (var-p nextc) (constit-p (var-values nextc)))
			     (constit-cat (var-values nextc))))))
	
	(if nextcat
	    (mapcar #'(lambda (entry) 
			(extend-arc entry (entry-name entry) arc))
		    (get-entries-by-position nextcat
					     (arc-end arc))))))))

;;  Special trace function for tracing entries

(defun trace-entry (entry)
  (if *user-trace-fn* 
      (apply *user-trace-fn* (list entry))
  (let ((id (entry-rule-id entry))
        (trace-feats (get-trace-features))
        (constit (entry-constit entry)))
    (pprint-logical-block (nil nil)
      (cond ((> (tracelevel) 0)
	     (pprint-newline :mandatory)
	     (format t "~%-Entering constituent ~S from ~S to ~S from rule ~S"
		     (entry-name entry)
		     (entry-start entry) 
		     (entry-end  entry)
		     id)
	     (format t ", prob = ~S, score = ~S" (entry-prob entry) (calculate-entry-score entry))
	     (pprint-newline :mandatory)
	     (when (or (> (tracelevel) 2) 
		       (member id (rules-to-trace))
		       (member (constit-cat constit) (constits-to-trace))
		       )
	       (format-constit constit trace-feats))
	     ;;	     (write-string "Done trace-entry")
	     (pprint-newline :mandatory)
	     )
	    ((or 
	      (member id (rules-to-trace))
	      (member (constit-cat constit) (constits-to-trace)))
	     (pprint-newline :mandatory)
	     (format t "-Entering2 constituent ~S from ~S to ~S" 
		     (entry-name entry)
		     (entry-start entry) 
		     (entry-end  entry))
	     (Format t ", prob = ~S, score = ~S, from rule ~S" (entry-prob entry) (calculate-entry-score entry) id)
	     (pprint-newline :mandatory)
	     (Format-constit constit trace-feats)
	     ;;   (write-string "Done trace-entry")
	     (pprint-newline :mandatory)
	     )
	    ))
    )))
	   

;; Special trace function for tracing arcs

(defun trace-arc (arc)
  (when (or (and (> (tracelevel) 3)
	         (nonLexicalConstit (arc-mother arc)))
	    (member (arc-rule-id arc) (rules-to-trace))
	    (member (constit-cat (arc-mother arc)) (constits-to-trace))
	    )
    (let ((trace-feats (get-trace-features)))

      (pprint-logical-block (nil nil)
	;;	(pprint-newline :mandatory)
	(format t "~:@_Adding active arc with prob ~S: " (arc-prob arc))
	(pprint-newline :mandatory)
	(format-term (arc-mother arc) trace-feats)
	(format t " ~S~:@_ " (arc-rule-id arc))
	(pprint-logical-block (nil (arc-pre arc))
	  (write-char #\Space)
	  (pprint-newline :fill );;stream
	  (loop
	    (format-constit (pprint-pop) trace-feats)
	    (pprint-exit-if-list-exhausted)
	    (write-char #\Space) 
	    (pprint-newline :fill );;stream
	    ))
	(format t "~%   * ")
	(pprint-newline :fill)	
	(pprint-logical-block (nil (arc-post arc))
	  (write-char #\Space)
	  (pprint-newline :fill );;stream
	  (loop
	    (format-term (pprint-pop) trace-feats)
	    (pprint-exit-if-list-exhausted)
	    (write-char #\Space) 
	    (pprint-newline :fill );;stream
	    ))
	(pprint-newline :mandatory)
	(format t "   from ~S to ~S" (arc-start arc) (arc-end arc))
	(pprint-newline :mandatory)
	))
    ))


