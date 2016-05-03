;;  This cocde has not been checked since th etransition to the NewIM architecture and therefore
;;    should not be trusted
(in-package :IM)

;;
;; load KB from kb.lisp
;;
(defvar *kb-file-directory* nil) ;; the directory in which kb.lisp exists
(defvar *kb-file* nil)           ;; the string representation of the file's location 
(defvar *kb-defs* nil)           ;; list representation of the file's contents
(defvar *KB-store* nil)          ;; a storage to put the kb info using the "kb" data structure

(defvar *use-predefined-kb-for-testing* t) ;; use kb.lisp?

;; 
;; a data structure to store KB info and make part of it readily accessible
;;
(defstruct kb  
  id          ;; ID
  class       ;; class
  name        ;; name, if any
  context     ;; LF context
  )
  
;; 
;; get the location of the kb.lisp
;;
(setq *kb-file-directory* (namestring #!TRIPS"src;Systems;obtw;"))
(setq *kb-file* (concatenate 'string *kb-file-directory* "kb.lisp"))

;; 
;; read the kb.lisp file
;;
(defun file-to-list (filename)
  ;;"put everything in a file into a list"
  (with-open-file (istream filename
			   :direction :input :if-does-not-exist nil)
		  (when istream		  
		    (do ((item (read istream nil nil nil) (read istream nil nil nil))
			 (results nil))
			((not item) (reverse results))
		      (setf results (cons item results))))))

;;
;; put the content of the kb file into a single list
;;
(setq *kb-defs* (file-to-list *kb-file*)) 

;; 
;; store the content of *kb-defs* into the kb data structure
;;
(defun store-kb-defs ()
  (mapcar (lambda (x) (store-kb-def x)) *kb-defs*))

(defun add-kb-def (def)
  (setq *kb-defs* (append *kb-defs* (list def)))
  (store-kb-def def))

(defun store-kb-def (def)
  (let* ((id (get-keyword-arg def :content))
	 (context (get-keyword-arg def :context))
	 (main-lf (get-def-from-akrl-context id context))
	 (class (get-keyword-arg main-lf :instance-of))
	 (name (get-keyword-arg main-lf :name)))
    (push (make-kb :id id :class class :name name :context context) *KB-store*)))

;;
;; call the store-kb-defs function
;;
(store-kb-defs)

;;
;; reload the KB defs
;;
(defun reload-kb ()
  (setq *KB-store* nil)
  (setq *kb-defs* (file-to-list *kb-file*))
  (store-kb-defs))

;; 
;; access the defintion of KB references stored in *kb-defs*
;;
(defun get-KB-def (id)
  (let ((lf nil)
	(context nil))
    ;; find the context where an LF with the ID is defined
    (setq context (get-keyword-arg (find-if (lambda (x) (setq lf (get-def-from-akrl-context id (get-keyword-arg x :context)))) *kb-defs*) :context))

    (if (and lf context) (list lf context))))

;;
;; get KB info stored in *KB-store* (using the kb structure)
;;
(defun get-KB-item (id)
  (find-if (lambda (x) (equal id (kb-id x))) *KB-store*))

;;
;; Assets in the obtw domain
;;
(defvar *asset-types* `(ELECTRICAL-CREW ONT::ELECTRICAL-CREW RESCUE-CREW ONT::RESCUE-CREW FIRE-CREW ONT::FIRE-CREW ENGINEERING-CREW ONT::ENGINEERING-CREW MEDICAL-CREW ONT::MEDICAL-CREW))

;;
;; Peform KB reference reasoning
;;
(defun Do-KB-reference (index)
  ;; do this KB reference only for OBTW
  (if (equal trips::*scenario* 'user::|obtw|)
      (let ((kb-items-to-focus (get-KB-items-based-on-gui-actions)))
	(if (null kb-items-to-focus) (setq kb-items-to-focus *KB-store*)) ;; if none, check the entire KB items

	;; now do KB reference
	(do-KB-reference-with-focused-items index kb-items-to-focus))))

(defun do-KB-reference-with-focused-items (index kb-items-to-focus) 
  (let* ((im-record (get-im-record index))
	 (referring-expressions (utt-record-referring-expressions im-record))
	 ;; collect all LFs from all referring expressions to get the whole context for this IM record
	 (ref-context (mapcar (lambda (x) (referent-lf x)) referring-expressions)))
    ;; process the referring expressions, and put the results in the REFERENT-KB-ASSOC-WITH field.
    (mapcar (lambda (x)
	      ;; for reach referent, set the kb-assoc-with field with a matching KB reference
	      (let* ((matching-kb-candidates nil)
		     (id (referent-id x))
		     (type (get-referent-type x))
		     (verb-info (third (find-if (lambda (x) (equal id (get-keyword-arg x :theme))) ref-context))) ;; a verb that has the current referent as its theme
		     (verb (if (listp verb-info) (second verb-info) verb-info)))
		(trace-msg 2 "KB referent info: ~S ~S ~S " id type verb)
		
		(cond
		  ;; when there is no associated verb
		  ((null verb)
		   ;; check if it's associated with a problem, then looks for related feeds
		   (let* ((assoc-info (third (find-if (lambda (x) (equal id (get-keyword-arg x :assoc-with))) ref-context))) ;; a verb that has the current referent as its theme
			  (assoc (if (listp assoc-info) (second assoc-info) assoc-info)))
		     (cond ((equal assoc 'ont::problem)
			    (mapcar (lambda (y)
				      (if (check-if-matching-problem-report type y)
					  (push y matching-kb-candidates)))
				    kb-items-to-focus))
			   
			   (t
			    (mapcar (lambda (y)
				      (let ((kb-ref-class (kb-class y)))
					(cond
					  ;; (1) check only the class name of a KB reference
					  ((string-equal (symbol-name type) (symbol-name kb-ref-class)) 
					   (push y matching-kb-candidates))
					  
					  ;; othewise
					  (t
					   ;; do nothing
					   ))))
				    kb-items-to-focus)))))
		  
		  ;; if there exists an associated verb, check the verb class and its context
		  ;; (1) show/bring/use/try X 
		  ;; (2) have X or X exists in the context of a WH question
		  ((or (om::subtype verb 'ont::show)
		       (om::subtype verb 'ont::cause-make)
		       (om::subtype verb 'ont::use)
		       (om::subtype verb 'ont::send)
		       (om::subtype verb 'ont::try)
		       (and (or (om::subtype verb 'ont::have)
				(om::subtype verb 'ont::exists)
				(om::subtype verb 'ont::have-property))
			    (equal 'ONT::SA_WH-QUESTION (third (find-if (lambda (z) (equal id (get-keyword-arg z :focus))) ref-context)))))
		   ;; check the class of the current KB reference
		   ;; NOTE:: in the future, the context of interaction should be taken into account (at the beginning, when there's no feeds windopw, it's more likely about opening an window)
		   ;; first, check based on the referent's LF type
		   (cond 
		     ;; in the case that the referent type is about templates
		     ((member type `(ont::template-info-object))
		      (mapcar (lambda (y)
				(let ((kb-ref-id (kb-id y))
				      (kb-ref-class (kb-class y)))
				  (if (equal kb-ref-class 'task-template)
				      (progn
					;; check if a template is about the template-info-object using the information contained in its modifiers or assoc-with
					;; NOTE:: (TBD) there can be multiple roles with the same role name (e.g., multiple :assoc-with): here it check only the first one (it seems working but it's not the right way.... :( 
					(let ((mods (get-keyword-arg (referent-lf x) :mods))
					      (assoc-with (get-keyword-arg (referent-lf x) :assoc-with)))
					  (if (or 
					       ;; in the case that modifiers exist
					       (and mods
						    (find-if (lambda (z)
							       (check-lf-match-against-kb-context z
												  (remove-unused-context (get-def-from-akrl-context z ref-context) ref-context)
												  (kb-id y)
												  (kb-context y)))
							     mods))
					       ;; in the case that there is an object associated with the template object
					       (and assoc-with
						    (check-lf-match-against-kb-context assoc-with
										       (remove-unused-context (get-def-from-akrl-context assoc-with ref-context) ref-context)
										       (kb-id y)
										       (kb-context y)))
					       ;; here, check if any of the modifiers matches the context of the task complete
					       ;; NOTE:: no single modifier context may match but collection of the modifiers' context may match or may be required to make a decision... TBD
					       ;; in the case that the verb has a modifier as a purpose
					       (let* ((verb-mods (get-keyword-arg (find-if (lambda (z) (equal id (get-keyword-arg z :theme))) ref-context) :mods))
						      ;; if there is a purpose instance, get its value
						      (verb-purpose (remove-if #'null (mapcar (lambda (z)
												(let* ((temp-lf (get-def-from-akrl-context z ref-context))
												       (temp-instance (third temp-lf)))
												  (if (listp temp-instance) (setq temp-instance (second temp-instance)))
												  (if (or (equal temp-instance 'ont::purpose)
													  (equal temp-instance 'ont::to))
												      (get-keyword-arg temp-lf :val))))
											      verb-mods))))
						 (find-if (lambda (z)
							    (check-lf-match-against-kb-context z
											       (remove-unused-context (get-def-from-akrl-context z ref-context) ref-context)
											       (kb-id y)
											       (kb-context y)))
							  verb-purpose))
					       ;; list other conditions below

					       )
					      (progn
						(trace-msg 2 "task template ~S matches the properties of modifiers ~S" kb-ref-id mods)
						(push y matching-kb-candidates))))))))
			      kb-items-to-focus))
		     
		     ((member type `(ont::group-object))
		      ;; get it name
		      (let ((name-info (get-keyword-arg (referent-lf x) :name-of))
			    (name-rep nil)
			    (name nil))
			;; make name-rep that is a list of strings and concatenate them
			(setq name-rep (if (stringp name-info) 
					   (list name-info) 
					   (if (listp name-info) 
					       (mapcar (lambda (y) (if (symbolp y) (symbol-name y) (format nil "~S" y))) name-info)
					       (list (format nil "~S" name-info)))))
			
			;; just in case, check if this group-object is equal to or associated with something that has a name; if so, append the name as well
			(let* ((name-extra (or 
					    (get-keyword-arg (referent-lf x) :eq)
					    (get-keyword-arg (referent-lf x) :assoc-with)))
			       (name-extra-lf (get-def-from-akrl-context name-extra ref-context))
			       (name-extra-name (get-keyword-arg name-extra-lf :name-of)))
			  (if (and name-extra-name (listp name-extra-name))
			      (mapcar (lambda (y) 
					(setq name-rep (append name-rep (list (format nil "~S" (get-alias-name y)))))) name-extra-name)))
			
			(mapcar (lambda (y) (setq name (concatenate 'string name y))) name-rep)
			
			(mapcar (lambda (y)
				  (let ((kb-ref-id (kb-id y))
					(kb-ref-class (kb-class y)))
				    (cond 
				      ((member kb-ref-class `(ELECTRICAL-CREW ONT::ELECTRICAL-CREW RESCUE-CREW ONT::RESCUE-CREW FIRE-CREW ONT::FIRE-CREW ENGINEERING-CREW ONT::ENGINEERING-CREW MEDICAL-CREW ONT::MEDICAL-CREW))
				       (let* ((pattern-info (list kb-ref-id 
								  (list 'akrl-pattern kb-ref-class '(:name ?name))
								  '(?name)))
					      (kb-instance-name (get-an-object-from-context-with-a-pattern (kb-context y) pattern-info)))
					 ;; check if the referent lf type's string representation overlaps with the target KB instance's string representation
					 (if (string-inclusion2 name kb-instance-name :skip-hyphen t)
					     (push y matching-kb-candidates))))
				      
				      (t
				       ;; do nothing
				       ))))
				kb-items-to-focus)))
		     
		     ((member type `(ont::truck ont::crew)) ;; a kind of assets
		      ;; check what it is associated with
		      (let* ((asset-vars (get-all-keywords-args (referent-lf x) `(:assoc-with :mods)))
			     (type-values nil))
			(mapcar (lambda (y)
				  (let ((kb-ref-id (kb-id y))
					(kb-ref-class (kb-class y)))
				    (cond 
				      ((member kb-ref-class *asset-types*)
				       ;; get type info with the asset property variables
				       (mapcar (lambda (z)
						 (let ((asset-info (third (get-def-from-akrl-context z ref-context))))
						   (setq type-values (append type-values (if (listp asset-info)
											     (cdr asset-info)
											     (list asset-info))))))
					       asset-vars)

				       ;; if there's no associated values for the type, use the type
				       (if (null type-values)
					   (setq type-values (get-equivalent-types type)))

				       ;; now look for the type values in KB
				       (let* ((pattern-info (list kb-ref-id 
								  (list 'akrl-pattern kb-ref-class '(:instance-of ?instance))
								  '(?instance)))
					      (kb-instance (get-an-object-from-context-with-a-pattern (kb-context y) pattern-info)))
					 ;; check if the referent lf type's string representation overlaps with the target KB instance's string representation
					 (if (find-if (lambda (z) 
							(string-inclusion2 (symbol-name z) (symbol-name kb-instance) :skip-hyphen t))
						      type-values)
					     (push y matching-kb-candidates))))
				      
				      (t
				       ;; do nothing
				       ))))
				kb-items-to-focus)))
		     
		     ((member type `(ont::asset))
		      ;; check if there's a quantifier
		      (cond ((equal (get-keyword-arg (referent-lf x) :quan) 'ont::universal)
			     ;; get all assets
			     (mapcar (lambda (y)
				       (let ((kb-ref-class (kb-class y)))
					 (cond 
					   ((member kb-ref-class `(ELECTRICAL-CREW ONT::ELECTRICAL-CREW RESCUE-CREW ONT::RESCUE-CREW FIRE-CREW ONT::FIRE-CREW ENGINEERING-CREW ONT::ENGINEERING-CREW MEDICAL-CREW ONT::MEDICAL-CREW))
					    (push y matching-kb-candidates))
				      
					   (t
					    ;; do nothing
					    ))))
				     kb-items-to-focus))

			    (t
			     ;; check KB element type for windows -- duplicated below
			     (mapcar (lambda (y)
				       (let ((kb-ref-id (kb-id y))
					     (kb-ref-class (kb-class y)))
					 (cond 
					   ((member kb-ref-class `(window ont::window))
					    ;; if this is about WINDOW, get its element type
					    (let* ((pattern-info (list kb-ref-id 
								       (list 'akrl-pattern kb-ref-class '(:contents :element-type ?type))
								       '(?type)))
						   (kb-instance (get-an-object-from-context-with-a-pattern (kb-context y) pattern-info))
						   (equivalent-types (get-equivalent-types type)))
					      (if (find-if (lambda (z)
							     ;; check if the referent lf type's string representation overlaps with the target KB instance's string representation
							     (string-inclusion2 (symbol-name z) (symbol-name kb-instance))) 
							   equivalent-types)
						  (push y matching-kb-candidates))))
					   
					   (t
					    ;; do nothing
					    ))))
				     kb-items-to-focus))))

		     ;; otherwise, check focusing on the KB reference's class
		     (t
		      (mapcar (lambda (y)
				(let ((kb-ref-id (kb-id y))
				      (kb-ref-class (kb-class y)))
				  (cond 
				    ((member kb-ref-class `(window ont::window))
				     ;; if this is about WINDOW, get its element type
				     (let* ((pattern-info (list kb-ref-id 
								(list 'akrl-pattern kb-ref-class '(:contents :element-type ?type))
								'(?type)))
					    (kb-instance (get-an-object-from-context-with-a-pattern (kb-context y) pattern-info))
					    (equivalent-types (get-equivalent-types type)))
				       (if (find-if (lambda (z)
						      ;; check if the referent lf type's string representation overlaps with the target KB instance's string representation
						      (string-inclusion2 (symbol-name z) (symbol-name kb-instance))) 
						    equivalent-types)
					   (push y matching-kb-candidates))))
				    
				    ((member kb-ref-class *asset-types*)
				     (let* ((pattern-info (list kb-ref-id 
								(list 'akrl-pattern kb-ref-class '(:instance-of ?instance))
								'(?instance)))
					    (kb-instance (get-an-object-from-context-with-a-pattern (kb-context y) pattern-info))
					    (equivalent-types (get-equivalent-types type)))
				       (if (find-if (lambda (z)
						      ;; check if the referent lf type's string representation overlaps with the target KB instance's string representation
							  (string-inclusion2 (symbol-name z) (symbol-name kb-instance)))
						    equivalent-types)
					   (push y matching-kb-candidates))))
				    
				    (t
				     ;; do nothing
				     ))))
			      kb-items-to-focus))))
		  
		  ;; (3) repair X
		  ((or (om::subtype verb 'ont::repair)
		       (om::subtype verb 'ont::managing))
		   (mapcar (lambda (y)
			     (if (check-if-matching-problem-report type y)
				 (push y matching-kb-candidates)))
			   kb-items-to-focus))
		  
		  ;; none of the above
		  (t
		   ;; do nothing
		   ))
		
		;; sort the matching candidates and select the best
		(trace-msg 2 "KB reference candidates ~S   " matching-kb-candidates)
		(find-matching-KB-candidates-and-make-association-with-a-referent matching-kb-candidates x)))
	    referring-expressions)))

;;
;; update kb-assoc-with
;;
(defun update-kb-assoc-with (ref assoc-with)
  (if (null (referent-kb-assoc-with ref))
      ;; only update when it's null
      (setf (referent-kb-assoc-with ref) assoc-with)))

;;
;; find matching KB candidates and make association with a referent
;;
(defun find-matching-KB-candidates-and-make-association-with-a-referent (kb-candidates target-ref)
  (let ((matching-kb-references (filter-kb-reference-candidates kb-candidates))
	(type (get-referent-type target-ref)))
    ;; update referent's kb-assoc-with attribute with the selected one
    (trace-msg 2 "KB reference ~S   " matching-kb-references)
    (if (and matching-kb-references (listp matching-kb-references))
	(cond ((< (list-length matching-kb-references) 2)
	       (update-kb-assoc-with target-ref (kb-id (first matching-kb-references))))
	      
	      (t
	       ;; check if the referent is an indefinite noun (not a set, though)
	       (cond ((and (equal 'ont::a (first (referent-lf target-ref)))
			   (null (equal 'ont::set type)))
		      ;; if so, pick one randomly...
		      (update-kb-assoc-with target-ref (kb-id (nth (random (list-length matching-kb-references)) matching-kb-references))))
		     
		     (t
		      (update-kb-assoc-with target-ref (mapcar (lambda (y) (kb-id y)) matching-kb-references)))))))))

;;
;; a special function to look for feeds about the problem specified by an lf type
;;
(defun check-if-matching-problem-report (lf-type ref)
  (let ((kb-ref-id (kb-id ref))
	(kb-ref-class (kb-class ref)))
    ;; find feeds about a problem that is related with the referent type (e.g., "power problem" for "power")
    (if (member kb-ref-class `(feed-item problem-report))
	;; if this is about FEED, check if it is related with the referent type (e.g., "power problem" for "power") and it is about a problem 
	(let* ((pattern-info (list kb-ref-id 
				   (list 'akrl-pattern kb-ref-class '(:problem-type :type ?type))
				   '(?type)))
	       (problem-type (get-an-object-from-context-with-a-pattern (kb-context ref) pattern-info))
	       (equivalent-types (get-equivalent-types lf-type)))
	  (find-if (lambda (temp-type)
		     ;; check if the problem type in the FEED has something about "problem" as well as the referent's LF type
		     (and (string-inclusion1 "problem" (symbol-name problem-type))
			  (string-inclusion1 (symbol-name temp-type) (symbol-name problem-type))))
		   equivalent-types)))))

;;
;; get equivalent types (e.g., assets -> resources) -- the return value is a list (that includes the original type)
;; this is to handle variant utterances with the same meaning (in the future, we should use ontology-based mapping using OM functions)
;;
(defun get-equivalent-types (type)
  (cond 
    ((equal type 'ont::resource)
     (list type 'ont::asset))
    
    ((equal type 'ont::tow-truck)
     (list type 'ont::engineering-crew))
    
    ((equal type 'ont::fire-truck)
     (list type 'ont::fire-crew))

    (t
     (list type))))

(defun get-alias-name (name)
  (cond 
    ((equal name 'ont::zero) 0)
    ((equal name 'ont::one) 1)
    ((equal name 'ont::two) 2)
    ((equal name 'ont::three) 3)
    ((equal name 'ont::four) 4)
    ((equal name 'ont::five) 5)
    ((equal name 'ont::six) 6)
    ((equal name 'ont::seven) 7)
    ((equal name 'ont::eight) 8)
    ((equal name 'ont::nine) 9)
    ((equal name 'ont::ten) 10)
    ((equal name 'ont::twenty) 20)
    (t name)))

(defun expand-name-value (names)
  (if (listp names)
      (let ((result nil))
	(mapcar (lambda (x)
		  (cond ((equal x 'w::clear-out)
			 (setq result (append result (list x 'w::clear))))
			
			(t
			 (setq result (append result (list x))))))
		names)

	;; return
	result)))

;;
;; filter kb candidates (TBD)
;; NOTE:: in filtering, aka sorting may need to be done (depending on the context: e.g., show all possible templates -> all templates)
;;
(defun filter-kb-reference-candidates (candidates)
  (remove-duplicates candidates :key #'(lambda (x) (kb-id x))))

;;
;; check if the LF in the referent context matches the given KB item context
;;
#|| e.g., check how the template's modifiers match the subtask parts of the KB context
;; LF context from IM records
((ONT::INDEF-SET ONT::V23927 (:* ONT::TEMPLATE-INFO-OBJECT W::TEMPLATE) :MODS (ONT::V23922 ONT::V23938))
 (ONT::F ONT::V23938 (:* ONT::ASSOC-WITH W::FOR) :OF ONT::V23927 :VAL ONT::V23952)
 (ONT::BARE ONT::V23952 (:* ONT::REPAIR W::RESTORE) :THEME ONT::V23955)
 (ONT::F ONT::V23922 (:* ONT::POSSIBLE W::POSSIBLE) :AFFECTED  ONT::V23963 :CONTENT ONT::V23927)
 (ONT::IMPRO ONT::V23963 ONT::REFERENTIAL-SEM :RELATED-TO ONT::V23922)
 (ONT::BARE ONT::V23955 (:* ONT::POWER W::POWER)))

;; KB context
((THE TT1 :INSTANCE-OF TASK-TEMPLATE :NAME "scout and restore" :FOR TT1-PURPOSE :ROLES (TT1-R1 TT1-R2) :SUBTASKS TT1-SUBTASKS)
 (A TT1-PURPOSE :INSTANCE-OF RESTORE-POWER-TASK)
 (A TT1-R1 :INSTANCE-OF SCOUT-ROLE :OF TT1 :VALUE TT1-R1VAL)
 (A TT1-R1VAL :INSTANCE-OF ELECTRICAL-CREW)
 (A TT1-R2 :INSTANCE-OF REPAIRER-ROLE :OF TT1 :VALUE TT1-R2VAL)
 (A TT1-R2VAL :TYPE ELECTRICAL-CREW)
 (A TT1-SUBTASKS :INSTANCE-OF SEQ :ELEMENT-TYPE TASK :MEMBERS (TT1-1 TT1-2))
 (A TT1-1 :INSTANCE-OF SCOUT-TASK :AGENT TT1-R1VAL :SUBTASKS TT1-1-SUBTASKS)
 (A TT1-2 :INSTANCE-OF RESTORE-TASK :AGENT TT1-R2VAL :SUBTASKS TT1-2-SUBTASKS)
 (A TT1-1-SUBTASKS :INSTANCE-OF SEQ :ELEMENT-TYPE TASK :MEMBERS (TT1-1-1 TT1-1-2 TT1-1-3)) 
 (A TT1-1-1 :INSTANCE-OF DRIVE-TO-TASK :AGENT TT1-R1VAL)
 (A TT1-1-2 :INSTANCE-OF ASSESS-TASK :AGENT TT1-R1VAL)
 (A TT1-1-3 :INSTANCE-OF REPORT-TASK :AGENT TT1-R1VAL)
 (A TT1-2-SUBTASKS :INSTANCE-OF SEQ :ELEMENT-TYPE TASK :MEMBERS (TT1-2-1 TT1-2-2 TT1-2-3))
 (A TT1-2-1 :INSTANCE-OF DRIVE-TO-TASK :AGENT TT1-R2VAL)
 (A TT1-2-2 :INSTANCE-OF FIX-TASK :AGENT TT1-R2VAL)
 (A TT1-2-3 :INSTANCE-OF RETURN-TO-BASE-TASK :AGENT TT1-R2VAL))
||#
(defun check-lf-match-against-kb-context (var context kb-id kb-context)
  ;; first, define a (recursive) function that checks if the instance class name has anything to do with the subtask in the given kb-context of a template
  (labels ((check-matching-task-with-names (subject task)
	     (find-if (lambda (x)
			(let ((subject-string-rep (if (symbolp x) (symbol-name x) (if (stringp x) x (format nil "~S" x))))
			      (task-string-rep (if (symbolp task) (symbol-name task) (if (stringp task) task (format nil "~S" task)))))
			  ;(trace-msg 2 "compare ~S and ~S" subject-string-rep task-string-rep)
			  (string-inclusion2 subject-string-rep task-string-rep)))
		      ;; when the subject a list, check all of its elements; if the subject is not a list (e.g., a string), check only that string
		      (expand-name-value (if (listp subject) subject (list subject)))))
	   (check-related-task (subject kb-context-var)
	     (let* ((subtask-role (get-role-from-akrl-context :subtasks kb-context-var kb-context))
		    (subtask-members (if subtask-role (get-role-from-akrl-context :members subtask-role kb-context))))
	       (find-if (lambda (x)
			  (let ((subtask-class (get-role-from-akrl-context :instance-of x kb-context)))
			    (or
			     ;; check if the subject has string similarity with the task name
			     (check-matching-task-with-names subject subtask-class)
			     ;; if no similarity found above, check its subtasks recursively
			     (check-related-task subject x))))
			subtask-members))))

    (let* ((lf-def (get-def-from-akrl-context var context))
	   (lf-class (third lf-def)))
      ;; when the instance class is ont::assoc-with, get its value's instance
      (if (equal (if (listp lf-class) (second lf-class) lf-class) 'ont::assoc-with)
	  (setq lf-class (third (get-def-from-akrl-context (get-keyword-arg lf-def :val) context))))

      ;; check if the lf class has anything to do with something in the kb context
      ;; here, check if lf-class is about ont::procedure (a broad concept)
      (if (and (listp lf-class) (equal 'ont::procedure (second lf-class)))
	  (let ((lf-word (symbol-name (third lf-class))))
	    ;; in this case, check the word part (e.g., the third element in (* ont::procedure w::scout-and-restore))
	    (multiple-value-bind (n1 n2) (get-string-match-offset "-and-" lf-word)
	      ;; check if the word contains "-and-"
	      (if (and (numberp n1) (numberp n2))
		  ;; there's "-and-" in the middle
		  ;; then, check if the strings before and after the "-and-" respectively have anything to do with the information in KB's task templates
		  (and (check-related-task (subseq lf-word 0 n1) kb-id)
		       (check-related-task (subseq lf-word n2) kb-id))
		  ;; otherwise, compare the entire word with the task info in task templates
		  (check-related-task lf-word kb-id))))
	  ;; otherwise, go on
	  (check-related-task lf-class kb-id)))))

;;
;; Check if the given referent LF type matches the information pointed by LF patterns
;; This function uses "akrl-match-into-context" function in util/util.lisp
;;
(defun get-an-object-from-context-with-a-pattern (context pattern-info)
  (let ((instance (car (akrl-match-into-context (first pattern-info) (second pattern-info) context (third pattern-info)))))
    (if (listp instance) 
	(second instance)
	instance)))

;;
;; Get the LF type of a referent in which the :lf-type role can be an atom (e.g., ont::feed) or a list (e.g., (* ont::feed w::feed))
;;
(defun get-referent-type (ref)
  (let ((type (referent-lf-type ref)))
    (if (listp type) (second type) type)))

;; -------------------------------------------------------------------
;;                << String utility functions >>
;; -------------------------------------------------------------------

;;
;; check if s1 is included in s2
;;
(defun string-inclusion1 (s1 s2 &key skip-hyphen)
  (if (and (stringp s1) (stringp s2))
      (multiple-value-bind (start end) (get-string-match-offset s1 s2 :skip-hyphen skip-hyphen)
	(and (numberp start) (numberp end)))))


;;
;; check if s1 or s2 includes the other one
;;
(defun string-inclusion2 (s1 s2 &key skip-hyphen)
  (or (string-inclusion1 s1 s2 :skip-hyphen skip-hyphen)
      (string-inclusion1 s2 s1 :skip-hyphen skip-hyphen)))

;;
;;
;; get where s1 starts/ends within s2
;;
(defun get-string-match-offset (s1 s2 &key skip-hyphen)
  ;; make both strings downcase
  (setf s1 (string-downcase s1))
  (setf s2 (string-downcase s2))

  (cond ((and (stringp s1) (stringp s2))
	 (labels ((ignorable (x) (or (is-whitespace x) 
				     (if skip-hyphen (equal x '#\-)) ;; ignore hyphen when it's explicitly mentioned to skip
				     ))
		  (non-ignorable (x) (null (ignorable x)))
		  (get-next-char-position-to-focus (x) 
		    (if (listp x)
			(let ((index 0)
			      (limit (- (list-length x) 1)))
			  (loop until(or (non-ignorable (nth index x)) (= index limit)) do (incf index))
			  index))))
	   ;; revise s1 to simplify computation by trimming some special characters in both ends
	   (if skip-hyphen
	       (setf s1 (string-trim `(#\Newline #\Tab #\Space  #\-) s1))
	       (setf s1 (string-trim `(#\Newline #\Tab #\Space) s1)))
	   
	   (let* ((s1-chars (make-char-list s1))
		  (s2-chars (make-char-list s2))
		  (s1-size (list-length s1-chars))
		  (s2-size (list-length s2-chars))
		  (index1 0)
		  (index2 0)
		  (match-sequence-started nil)
		  (start-offset nil)
		  (end-offset nil)
		  (found nil))
	     (loop while (and (null found) 
			      (< index2 s2-size))
		do
		;; first skip whitespaces and hyphens
		  (setf index1 (+ index1 (get-next-char-position-to-focus (subseq s1-chars index1))))
		  (setf index2 (+ index2 (get-next-char-position-to-focus (subseq s2-chars index2))))
		  
		  (let ((c1 (nth index1 s1-chars))
			(c2 (nth index2 s2-chars)))
		    (cond ((eql c1 c2)
			   ;; if this is the first match
			   (if (null match-sequence-started)
			       (progn
				 (setf match-sequence-started t)
				 (setf start-offset index2)))

			   (incf index1)
			   (incf index2))
			  
			  (t ;; no match! -- reset some variables including the starting position of s1
			   (setf match-sequence-started nil) 
			   (setf index1 0)
			   (incf index2)))

		    (if (and match-sequence-started (<= s1-size index1))
			(progn 
			  (setf found t)
			  (setf end-offset index2)))))
	     
	     (if (null found) 
		 (progn
		   (values nil nil))
		 (values start-offset end-offset)))))

	(t
	 (values nil nil))))

(defun make-char-list (somestring)
  (let (x)
    (dotimes (i (length somestring))
      (setq x (append x (list (aref somestring i)))))
    x))

(defun is-whitespace (char)
  (member char `(#\Newline #\Tab #\Space)))

(defun include-period (text)
  (member #\. (make-char-list text)))

(defun get-all-keywords-args (lf keys)
  (if (and (listp lf) (listp keys))
      (let ((args nil)
	    (result nil))
	(setq args (mapcar (lambda (x) (get-all-keyword-args lf x)) keys))
	(mapcar (lambda (x)
		  (if (listp x)
		      (setq result (append result x))
		      (setq result (append result (list x)))))
		args)
	result)))

;;
;; get all role values for a given role (i.e., keyword), addressing cases with same multiple roles in an LF
;;
(defun get-all-keyword-args (lf key)
  (if (and (listp lf) (keywordp key))
      (let ((counter 0)
	    (temp-val nil)
	    (result nil)
	    (limit (list-length lf)))
	(loop while(< counter limit)
	     do
	     (if (and (keywordp (nth counter lf)) 
		      (equal (nth counter lf) key)
		      (< (+ 1 counter) limit))
		 (progn
		   (setq temp-val (nth (+ 1 counter) lf))
		   (if (null (keywordp temp-val))
		       (progn
			 (setq result (append result (if (listp temp-val) temp-val (list temp-val))))
			 (incf counter)))))
	     (incf counter))
	result)))
