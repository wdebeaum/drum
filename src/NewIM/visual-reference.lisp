
(in-package :IM)

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

(defun Do-visual-reference (index)
  (let* ((im-record (get-im-record index))
	 (referring-expressions (utt-record-referring-expressions im-record))
	 ;; collect all LFs from all referring expressions to get the whole context for this IM record
	 (ref-context (mapcar (lambda (x) (referent-lf x)) referring-expressions)))
    ;; process the referring expressions, and put the results in the REFERENT-REFERS-TO field.
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