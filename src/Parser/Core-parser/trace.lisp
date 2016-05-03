;;
;; trace.lisp
;;
;; Time-stamp: <Wed Jul  1 06:20:25 EDT 2015 jallen>
;;
;; History:
;;   ?????? james   - written as part of Chart.lisp.
;;   941215 ringger - moved to own file trace.lisp.
;;

(in-package "PARSER")

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

;;=============================================================================
;;  TRACING
;;
;; There are two levels of tracing:
;; 1.  Basic tracing: each entry is traced as it is entered,
;;     and the complete chart is printed at the end (the default on)
;; 2.  Verbose tracing: each non-lexical active arc is traced as well
;;     as it is constructed
;; TRACEON enables simple tracing, TRACEOFF turns it off
;; VERBOSEON enables verbose tracing, if simple tracing is on.

;;   trace: 0 - no tracing, 1 - for basic tracing, 2 - verbose tracing

;;; New trace code

(defvar *step* nil)
(defvar *trace-level* 0)
(defvar *rules-traced* nil)
(defvar *trace-feats* t)
(defvar *constits-traced* nil)
(defvar *user-trace-fn* nil)    ;; if this is set, this function is called rather than the default printing
                                ;;  currently, this only occurs on constituent entry, and the code is in Chart.lisp

(defun traceon (n)
  (if (numberp n)
      (setq *trace-level* n)
    (parser-warn "Step level must be a number")))

(defun traceoff nil
  (setq *trace-level* 0)
  (setq *step* nil)
  (setq *rules-traced* nil)
  (setq *constits-traced* nil))

(defun step-off nil
  (setq *step* nil))

(defun step-on (n)
  (setq *step* t)
  (traceon n))

(defun tracelevel nil
  *trace-level*)

(defun trace-msg (n &rest args)
  (when *trace-level*
    (when (>= *trace-level* n)
      (apply #'format (cons t args))
      (when *step*
	(format t "~%     at level ~S, change?:" *trace-level*)
	(let ((x (read-line)))
	  (if (not (string= x ""))
	      (let ((new (read-from-string x)))
		(if (numberp new) (setq *trace-level* new))))))))
  (values))

(defun trace-rule (&rest rule-ids)
  (let ((ids (get-rule-ids)))
    (when rule-ids
	(let ((new-ids
	       (mapcar #'(lambda (x) 
			   (if (not (member x ids))
			       (parser-warn "no rule with id ~S" x)
			     x))
		(util::convert-to-package rule-ids :w))))
	(setq *rules-traced* (append new-ids *rules-traced*))))))

(defun trace-constit (&rest constits)
  (setq *constits-traced* (append constits *constits-traced*))
  )
  
(defun rules-to-trace ()
  *rules-traced*)
  
(defun constits-to-trace ()
  *constits-traced*)
  
(defun set-trace-features (feats)
  (setq *trace-feats* feats))

(defun get-trace-features nil
  *trace-feats*)


;;  BREAK POINTS 

(let ((stopping-condition nil))

  (defun break-on (c)
    (if (null c)
	(setq stopping-condition nil)
      (let ((constit (read-embedded-constit c nil)))
	(if (constit-p constit)
	    (setq stopping-condition constit)
	  ))))
  
  (defun get-stopping-condition ()
    STOPPING-CONDITION)
  
  ) ;; end scope of STOPPING-CONDITION

;; Specific unification debugging facility
;;
;; This allows the user to match a constituent on the chart with all arcs
;; in the chart
;;  and beginning arcs in the grammar and produces an analysis of why the
;; unification fails
;;  in each case.

(defvar FAILURE-TRACE nil)

(defun debug-match-arcs (entry-name)
  "Unifies entry against every possible arc in chart and reports reason for
failure"
  (let
    ((entry (get-entry-by-name entry-name))
     (failure-trace t))
    (if (entry-p entry)
      (let* ((start-pos (entry-start entry))
             (all-arcs (get-arcs start-pos))
             (FAILURE-TRACE t)
	     (cat (constit-cat (entry-constit entry)))
	     ;; remove those whose initial CAT doesn't match
	     (arcs (remove-if-not #'(lambda (a) (let ((e (car (arc-post a))))
						  (or (var-p e)
						      (and (constit-p e) (or
									  (eq (constit-cat e) cat)
									  (var-p (constit-cat e))))))) all-arcs)))
	(mapc #'(lambda (x)
		    (pprint-logical-block (nil nil)
		      ;;(pprint-newline :fill)
		      (format t "~%Checking arc from rule ~W from ~S ~S" (arc-rule-id x) (arc-start x) (getsubconstitnames 1 (arc-mother x)))
		      (test-arc entry x)
		      (pprint-newline :mandatory)
		      ))
                arcs))))
  (values))

(defun debug-match-grammar (entry-name)
  "Unifies entry against every rule in grammar"
  (let
    ((entry (get-entry-by-name entry-name))
     (failure-trace t))
    (if (entry-p entry)
      (let*
        ((c (entry-constit entry))
	 ;;(start (entry-start entry))
	 ;;(end (entry-end entry))
         (FAILURE-TRACE t))
        (mapc #'(lambda (x)
		    (pprint-logical-block (nil nil)
		      (pprint-newline :fill)
		      (Format t "~% Matching rule ~S" (rule-id x))
		      (multiple-value-bind (bndgs prob)
			  (Constit-Match (car (rule-rhs x)) C)
			(if bndgs
			    (Format t "    SUCCESS! with prob ~S" prob))
			)
		      (pprint-newline :mandatory))
		    )
	      (lookup-rules c))))
    )
  nil)

(defun test-arc (entry  arc)
    (multiple-value-bind (bndgs prob)
	(match-next-pattern-in-rhs (arc-post arc) (entry-constit entry))
      (when bndgs 
        (multiple-value-bind 
          (rhss new-bndgs)
          (gen-new-rhsides (subst-in (cdr (arc-post arc)) bndgs) bndgs)
         (If new-bndgs
	    (pprint-logical-block (nil nil)
	      (let ((arcprob (* (if (numberp prob) prob 1) (arc-prob arc) (entry-prob entry))))
		(if (null (first rhss))

		    (format t "~:@_         SUCCESS AND RULE COMPLETE with prob ~S; ~S~:@_" arcprob (length-entry-score (make-entry :end (entry-end entry) :start (arc-start arc))))
		    (format t "~:@_         SUCCESS AND RULE EXTENDED with prob ~S; ~S~:@_" arcprob (length-entry-score (make-entry :end (entry-end entry) :start (arc-start arc)))))
	        ))))
          )))
	    

(defun failure-message (v1 v2 f)
  (pprint-logical-block (nil nil)
    (pprint-newline :fill)
    (format *standard-output* "   Failed matching feature ~S on values: " f)
     (cond
     ((member f '(w::sem w::argsem))
      (format-var-values v1 nil *standard-output*)
      (format *standard-output* " and ")
      (format-var-values v2 nil *standard-output*)
      )
     (t 
      (format-term v1 nil *standard-output*)
      (format *standard-output* " and ")
      (format-term v2 nil *standard-output*)
      ))
    (pprint-newline :mandatory)
    ))
        
 
