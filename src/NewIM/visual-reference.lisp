;;  THIS FILE has code to interface with the external situation
;;    which could include a representation of a GUI or a model of the actual perceived world
;;   Also, it provides connects with tagging and classification of phrases
;;   by external resources that are not handled by texttagger, which does word-level tagging

(in-package :IM)

(defvar *external-phrase-tagger* nil)

(defun Do-visual-reference (index)
  "This supports any external tagger as determiend by *external-phrase-tagger* variable. Each
    requires some specialized code that also should be in this file"
  (let* ((im-record (get-im-record index))
	 (referring-expressions (utt-record-referring-expressions im-record))
	 ;; collect all LFs from all referring expressions to get the whole context for this IM record
	 (ref-context (mapcar (lambda (x) (referent-lf x)) referring-expressions))
	 )
   
    (case *external-phrase-tagger*
      (variable-finder
       (mapcar #'(lambda (x)
		   (identify-parameters x ref-context))
	       referring-expressions)))
      ))

(defun identify-parameters (ref context)
  (let* ((lf (referent-lf ref))
	(input (or (referent-input ref) (if (consp (third lf))
					    (cddr lf)))))
    (when (not (or (member (car lf) '(ont::speechact ont::pro ont::pro-set))))
      (let* ((type (get-lf-type lf))
	     (newlf 
	      (cond 
		((om::subtype type 'ONT::quantity)
		 nil)
		((om::subtype type 'ONT::geo-object)
		 (tag-geo-object lf input context))
		((om::subtype type 'ont::YEAR)
		 (tag-and-install-info 'find-var lf '(YEAR) context))
		((om::subtype type 'ont::number)
		 nil)
		((or (om::subtype type 'ONT::DOMAIN)
		     (om::subtype type 'ont::value)
		     (om::subtype type 'ont::quantity-abstr)
		     (om::subtype type 'ONT::STATUS))
		 (tag-and-install-info 'find-var lf input context))
		((or (om::subtype type 'ONT::SUBSTANCE)
		     (om::subtype type 'ONT::DOMAIN-PROPERTY)
		     )
		 (tag-and-install-info 'find-code lf input context))
		)))
	
	(when newlf
	  (setf (referent-lf ref) newlf))))))

(defun tag-geo-object (lf input context)
  (let* ((reply (send-and-wait `(request :content (get-geographic-region
						  :description ,(stringify input)
						  :format (code "ISO")
						  ))
			       ))
	 (reply-content (find-arg-in-act reply :content)))
    (case (car reply-content)
      (answer
       (let ((code (find-arg-in-act reply-content :location)))
	 (if code
	     (append lf `(:param-code (match :variable ((TRIPS LOC))
					     :code ((TRIPS ,code)) :score 1)))
	     lf)))
      (otherwise lf))))

(defun stringify (x)
  "this takes a symbol or list of symbols and makes a strong out of the symbol names"
  (if (symbolp x)
      (symbol-name x)
      (if (consp x)
	  (if (consp (cdr x))
	      (concatenate 'string (stringify (car x)) " " (stringify (cdr x)))
	      (stringify (car x))
	  ))))
      
    

						 
(defun tag-and-install-info (fn lf input context)
  (when input
    (let ((reply (send-and-wait `(REQUEST :content (,fn
						    :phrase ,(strip-prep input) :term ,lf :content :context
						    :top-n 3)))))
      (case (car reply)
	(ANSWER
	 ;;(format t "~% Found answer: ~S" reply)
	 (let* ((matches  (remove-if #'(lambda (x)
					 (let ((score (find-arg-in-act x :score)))
					   (or (null score)
					       (not (numberp score))
					       (< score .5))))
				     (cdr reply))))
	   (if matches 
	       (append lf (list :param-code
				(or (find-best-trips-match (if (eq fn 'find-code) :code :variable) matches)
				    (car matches))))
	       )
	   ))
	))))

(defun find-best-trips-match (slot matches)
  "finds the first match that identifies a TRIPS variable"
  (when matches
    (let ((codes (find-arg-in-act (car matches) slot)))
      (if (assoc 'trips codes)
	  (car matches)
	  (find-best-trips-match slot (cdr matches))))))

(defun strip-prep (input)
  "strips off a proposition at the start if it has no semantic content"
  (if (member (car input)
	      '(w::of w::to w::in W::by))
      (mapcar #'put-in-im-package (cdr input))
      (mapcar #'put-in-im-package input)))

(defun put-in-im-package (w)
  (convert-to-package w *im-package*))

  #|| ;; process the referring expressions, and put the results in the REFERENT-REFERS-TO field.
    ;; for the time being, do this KB reference only for OBTW
    (if (equal trips::*scenario* 'user::|obtw|)
	(mapcar (lambda (x)
		  ;; for reach referent, set the kb-assoc-with field with a matching KB reference
		  (let* ((matching-kb-candidates nil)
			 (id (referent-id x))
			 (type (get-referent-type x))
			 (verb-info (third (find-if (lambda (x) (equal id (get-keyword-arg x :theme))) ref-context))) ;; a verb that has the current referent as its theme
			 (verb (if (listp verb-info) (second verb-info) verb-info)))
		    (trace-msg 2 "Visual referent info: ~S ~S ~S " id type verb)

		    (cond
		      ;; let's work on X
		      ((or (om::subtype verb 'ont::working)
			   )
		       (cond ((member type `(ont::referential-sem ont::problem))
			      ;; check if there's any problem-related stuff to work on that was visually reported to the user
			      (let* ((problems-to-focus (fiter-reported-problems-to-focus x ref-context (get-reported-problems-to-focus *problem-lookback-distance*)))
				     ;; at the moment, pick the first (i.e., the most recent) problem if there are multiple items
				     (target-kb (get-KB-item (get-keyword-arg (first problems-to-focus) :id))))
				(if target-kb (push target-kb matching-kb-candidates))))
			     
			     ;; otherwise
			     (t
			      ;; do nothing
			      )))

		      (t
		       ;; do nothing
		       ))

		    ;; sort the matching candidates and select the best
		    (trace-msg 2 "Visual reference candidates ~S   " matching-kb-candidates)
		    (find-matching-KB-candidates-and-make-association-with-a-referent matching-kb-candidates x)))
		referring-expressions))))

||#

;;  INTERFACING WITH A GUI  (obsolete code to be updated when needed)

;;
;; data structures and functions to access them
;;

(defvar *problem-reports* nil)             ;; problems visually reported to a user
(defvar *problem-lookback-distance* 3)     ;; a static distance that limits how far to look back in finding a problem to focus

(defun clear-visual-reference-DS ()
  (setf *problem-reports* nil)
  )

;;
;; functions to record UI actions
;; NOTE:: using these functions, we track what is visible to a user and what a users selects in GUIs
;;        we need to maintain aka stacks to track user GUI actions that are related (e.g., open a feed window and selec a feed) and we may also need to pop multiple (related) actions when a new action nullifies the previous GUI actions, making them obsolete...
;;
(defun handle-ui-action (act-info)
  (declare (ignore act-info)))

(defun handle-cps-action (cps-act-info)
  (declare (ignore cps-act-info)))

;;
;; process new-report messages
;; 
#|| example
  (TELL :CONTENT (NEW-REPORT :SENDER CREW-GL-2 
                    :TEXT "Road blocked by the road blockage at latitude 43.130801 longitude -77.665853"
		    :PROBLEM (ROAD-BLOCKAGE :ID PROBLEM-3 
                                            :INTERSECTION (INTERSECTION :ID I212630502 :X 332.01562 :Y 325.57642 :LON -77.665855 :LAT 43.130802)
				            :EFFORT-REQUIRED 40.0)))
||#
(defun handle-gui-report-notification (report-info)
  (let* ((sender (get-keyword-arg report-info :sender))
	 (text (get-keyword-arg report-info :text))
	 (problem (get-keyword-arg report-info :problem))
	 (type (first problem))
	 (id (get-keyword-arg problem :id))
	 (intersection (get-keyword-arg problem :intersection))
	 (effort-level (get-keyword-arg problem :effort-required))
	 (loc-id (get-keyword-arg intersection :id))
	 (x (get-keyword-arg intersection :x))
	 (y (get-keyword-arg intersection :y))
	 (lon (get-keyword-arg intersection :lon))
	 (lat (get-keyword-arg intersection :lat)))
    ;; make a KB definition and add it to KB data structures defined in KBreference.lisp
    ;; when using kb.lisp, skip adding this new info into kb
    (add-kb-def `(define :content ,id
		   :context ((a ,id :instance-of problem-report :reporter ,sender :text ,text :problem-type prob-type0 :location prob-loc0 :effort-required ,effort-level)
			     (the prob-type0  :instance-of problem-type :type ,type)
			     (the prob-loc0 :instance-of location :id ,loc-id :x ,x :y ,y :lon ,lon :lat ,lat))))

    ;; save the problem ID
    (save-reported-problem id)))


;;
;; save a reported problem
;;
(defun save-reported-problem (id) 
  (push `(problem :id ,id :index ,*im-utt-count*) *problem-reports*))

;;
;; get reported problem to focus
;; NOTE:: currently, only the recent ones based on a static distance -- in the future, more context (including what was reported, what was processed, if it's still visible, etc.) should be taken into account
;;
(defun get-reported-problems-to-focus (distance-to-look-back)
  (let ((utt-count-to-cut (- *im-utt-count* distance-to-look-back)))
    (remove-if #'null (mapcar (lambda (x) (if (<= utt-count-to-cut (get-keyword-arg x :index)) x)) *problem-reports*))))

;;
;; filter problems to focus based on the referent type
;;
(defun fiter-reported-problems-to-focus (ref context problems-to-focus)
  (let ((type (get-referent-type ref)))
    (if (equal type 'ont::problem)
	;; check if there's anything associated with it -- if so, check if the problems are related with the associated object
	(let* ((assoc-var (get-keyword-arg (referent-lf ref) :assoc-with))
	       (assoc-info (get-def-from-akrl-context assoc-var context))
	       (assoc-type-info (third assoc-info))
	       (assoc-type (if (listp assoc-type-info) (second assoc-type-info) assoc-type-info)))
	  (remove-if #'null (mapcar (lambda (x) (check-if-matching-problem-report assoc-type (get-KB-item (get-keyword-arg x :id)))) problems-to-focus)))
	;; otherwise, return the problems as they are
	problems-to-focus)))

;;
;; find KB items based on user's gui actions
;;
(defun get-KB-items-based-on-gui-actions ()

  )


;;   Phrase based Tagging

