;;;  This file contains functions for use in the "munger", but which are not
;;;  important in an understanding of the "munger's" operation.

;______________________________________________________________________________
;; Declaration of being in the parser package.

(in-package parser)

;______________________________________________________________________________
;;;  Other helpful functions - to be put in a background functions file

;;; Evaluative functions

;; Match checks that all the (other) edges in a template do not "clash" with
;; the constraints on a speech-act.  So, for each constraint in the speech act,
;; either it does not fill a structural role in the speech-act, or it is an 
;; edge in the template.
#||
(defun match (template speech-act)
  (let ((edgest (mapcar #'(lambda (x) (edge-type x)) (template-edges template)))
	(edgess (find-edges (SA-semantics speech-act)))
	(result t))
    (dolist (element edgess result)
	    (if (member (first element) '(:LSUBJ :LOBJ :LCOMP :LIOBJ))
		(if (not (member (first element) edgest))
		  (setf result nil))))))

;; 

(defun set-equal (set1 set2 &key (test #'equal))
  (and (equal (length set1) (length set2))
       (let ((result t))
	 (dolist (element set1 result)
	   (if (not (member element set2 :test test))
	       (setf result nil))))))

;; Use this to check if the semantic forms of two speech-acts are equal.

(defun sem-equal (SA1 SA2)
  (cond ((not (SA-semantics SA1)) (when (not (SA-semantics SA2)) t))
	((propp (SA-semantics SA1)) 
	 (when (propp (SA-semantics SA2))
	   (make-lookup-table (list SA1 SA2))
	   (let ((temp1 (mapcar #'(lambda (x) (list (first x) (find-cl (third x)))) (find-edges (SA-semantics SA1))))
		 (temp2 (mapcar #'(lambda (x) (list (first x) (find-cl (third x)))) (find-edges (SA-semantics SA2)))))
	     (if (and (match-verb (rest (find-cl (Sa-semantics SA1))) (rest (find-cl (SA-semantics SA2))))
		      (set-equal temp1 temp2))
		 t))))
	((not (listp (SA-semantics SA1))) 
	 (when (not (listp (SA-semantics SA2)))
	   (make-lookup-table (list SA1 SA2))
	   (let ((temp (mapcar #'(lambda (x) (list 'V (find-cl x))) 
			       (list (SA-semantics SA1) (SA-semantics SA2)))))
	     (equal (first temp) (second temp)))))))

;; Use this to check if two CCA's are "equal" (have SA's which are sem-equal in
;; the same order, and have the same number of SA's).

(defun CCA-equal (CCA1 CCA2)
  (cond ((and (cca-p CCA1) (cca-p CCA2))
	 (set-equal (CCA-acts CCA1) (CCA-acts CCA2) :test #'sem-equal))
	(t
	 NIL)))

;;; These are used to check equality over a whole speech act.  
;; All-equal checks if the parts of two speechacts are equal.


(defun all-equal (act1 act2)
  (cond
   ((and (cca-p act1) (cca-p act2) (equal act1 act2)) t)
   ((and (cca-p act1) (cca-p act2))
    (let ((temp1 (remove-vars act1))
	  (temp2 (remove-vars act2)))
      (cca-equalparts temp1 temp2)))
   ((and (sa-p act1) (sa-p act2) (equal act1 act2)) t)
   ((and (sa-p act1) (sa-p act2))
    (let ((temp1 (remove-vars act1))
	  (temp2 (remove-vars act2)))
      (sa-equalparts temp1 temp2)))
   (t nil)))

;; Cca-equalparts checks if the parts of two cca's are equal (note:  order
;; of SA's in CCA-acts slot is not important).

(defun cca-equalparts (act1 act2)
  (and (set-equal (CCA-acts act1) (CCA-acts act2) :test #'sa-equalparts)
       (equal (CCA-reliability act1) (CCA-reliability act2))
       (equal (CCA-mode act1) (CCA-mode act2))
       (equal (CCA-noise act1) (CCA-noise act2))))

;; Sa-equalparts checks if the parts of two sa's are equal (note:  order
;; of objects in SA-objects slot and SA-semantics slot is not important).    

(defun sa-equalparts (act1 act2)
  (and (equal (SA-type act1) (SA-type act2))
       (equal (SA-focus act1) (SA-focus act2))
       (set-equal (SA-objects act1) (SA-objects act2) :test #'equal)
       (cond ((and (propp (SA-semantics act1)) (propp (SA-semantics act2)))
	      (and (equal (find-cl (SA-semantics act1)) (find-cl (SA-semantics act2)))
		   (set-equal (find-edges (SA-semantics act1))
			 (find-edges (SA-semantics act2)))))
	     ((and (not (propp (SA-semantics act1))) (not (propp (SA-semantics act2))))
	      (equal (SA-semantics act1) (SA-semantics act2)))
	     (t nil))
       (equal (SA-noise act1) (SA-noise act2))
       (equal (SA-social-context act1) (SA-social-context act2))
       (equal (SA-reliability act1) (SA-reliability act2))
       (equal (SA-mode act1) (SA-mode act2))
       (equal (SA-syntax act1) (SA-syntax act2))
       (equal (SA-setting act1) (SA-setting act2))
       (equal (SA-input act1) (SA-input act2))))


;; Returns t if the sem-form is a proposition

(defun propp (form)
  (and (listp form) (equal (first form) ':PROP)))

;; Returns t if the var points to an object of :STATUS :PRO.

(defun pronounp (var)
  (let ((temp (lookup var)))
    (when temp
	(when (equal (second (assoc ':STATUS (rest (rest temp)))) ':PRO)
	      t))))
	

;; Checks if a "template" verb class matches the actual verb class.
;; May be a bit over-inclusive just now.
(defun match-verb (idealverb realverb)
  (when (member realverb 
     (list nil idealverb
	  (list ':WILL idealverb)
	  (list ':SHALL idealverb)
	  (list ':DO idealverb)
	  (list ':CAN idealverb)
          (list ':NOT (list ':DO idealverb))) :test #'equal) t))


;;; Transforming functions 

;;; These functions take all the variables out of a speech act and replace them
;;; with V.
;; Remove variables from a speech act.

(defun remove-vars (act)
  (cond ((cca-p act) (remove-cca-vars act))
	((sa-p act) (remove-sa-vars act))
	(t (error "Non-speech-act passed to remove-vars"))))

;; Remove variables from a CCA.

(defun remove-cca-vars (act)
  (setf (CCA-acts act) (mapcar #'remove-sa-vars (CCA-acts act)))
  act)


;; Remove variables from an SA.

(defun remove-sa-vars (act)
  (setf (SA-focus act) (remove-var (SA-focus act)))
  (setf (SA-objects act) (mapcar #'remove-vars-from-list (SA-objects act)))
  (setf (SA-semantics act) (remove-vars-from-list (SA-semantics act)))
  (setf (SA-syntax act) (remove-vars-from-list (SA-syntax act)))
  act)

;; Remove variables from a list of things.

(defun remove-vars-from-list (l)
  (cond ((not l) nil)
	((not (listp l)) (remove-var l))
        ((listp (first l)) (cons (remove-vars-from-list (first l)) (remove-vars-from-list (rest l))))
	(t (cons (remove-var (first l)) (remove-vars-from-list (rest l))))))

;; Replace an item with V if it is a variable.

(defun remove-var (item) 
  (if (varp item) 'V item))

;; This returns all the templates of a certain class of verb.

(defun find-templates (sem-form)
  (declare (special *templates*))
  (let ((result nil))
    (dolist (element *templates* result)
      (when (match-verb (template-type element) (rest (find-cl sem-form)))
        (push element result)))))
    
;; If a compound communications act only has one speech act left inside it, 
;; just return the speech act.

(defun find-right-type (ccas)
  (cond ((not ccas) nil)
	((= (length (CCA-acts (first ccas))) 1)
	 (cons (first (CCA-acts (first ccas))) (find-right-type (rest ccas))))
	(t (cons (first ccas) (find-right-type (rest ccas))))))  
   

;; If we are adding a new constraint to a form which had only one constraint,
;; we need to insert an :AND as well.  We also need to check that all the
;; variables are correct.

(defun check-edge (sem-form) 
  (let ((name (second (assoc ':VAR (rest sem-form)))) (edges (find-edges sem-form)) (result nil))
    (dolist (element edges result)
      (if (not (equal (second element) name))
	  (setf result (cons (list (first element) name (third element)) result))
	  (setf result (cons element result))))
    (setf sem-form (swap (swap result edges (assoc ':CONSTRAINT (rest sem-form))) (assoc ':CONSTRAINT (rest sem-form)) sem-form)))
  (if (> (length (assoc ':CONSTRAINT (rest sem-form))) 2)
      (setf sem-form (swap (list ':CONSTRAINT (cons ':AND (rest (assoc ':CONSTRAINT (rest sem-form)))))
			   (assoc ':CONSTRAINT (rest sem-form))
			   sem-form))
    sem-form))

;; Given a CCA, this adjusts the non-acts slots of the CCA.  Currently, the 
;; only slot which might need adjusting is the reliability slot.

(defun adjust (cca n)
  (let ((sum 0)
	(cca1 (cdr cca)))  
    (dolist (item (CCA-acts cca1) sum)
      (setf sum (+ sum (SA-reliability item))))
    (setf (CCA-reliability cca1) (float (/ sum (length (CCA-acts cca1)))))
    (cons (cons n (first cca)) cca1)))

;; This sorts a list of compound communications acts by whatever "best" 
;; measure we define.  Currently, the measures are the average number of 
;; speech acts skipped to get a combination, and the number of speech-acts
;; in a CCA.

(defun best-measure (ccas) 
  (let* ((sorted-by-skips
	   (mapcar 'cdr
		   (stable-sort (mapcar #'(lambda (x)
					    (cons (average (first x)) (rest x))) ccas)
				#'< :key #'car)))
    (sorted-by-no-acts 
           (mapcar 'cdr
                   (stable-sort (mapcar #'(lambda (x)
			                    (cons (length (CCA-acts x)) x))
				        sorted-by-skips)
			         #'< :key #'car))))
      sorted-by-no-acts))

  
;; Given a new node for an edge, the old edge, and the semantic form in which
;; to insert the edge, this performs the insertion.

(defun alter-edge (newpart edgetype sem-form) 
  (let ((temp (swap 
		(list edgetype (second (assoc ':VAR (rest sem-form))) newpart)
		(assoc edgetype (find-edges sem-form))
		(find-edges sem-form))))
    (if (and (> (length temp) 1) (< (length (find-edges sem-form)) 2))
	(swap (list (cons ':AND temp))
	      (find-edges sem-form)
	       sem-form)
      (swap temp (find-edges sem-form) sem-form))))



;;; Functions which find something in a complex structure

;; This takes some semantic/logical form, and returns a cons pair consisting
;; of the type of the form (description, path, predicate, proposition) and
;; the class of the form (e.g. train, any-sem, agent).  A variable is a 
;; semantic/logical form.

(defun find-cl (form)
  (cond ((equal form ':*YOU*)
         '(:DESCRIPTION . :AGENT))
        ;;((var form) (find-cl (rest (lookup form))))
	((listp form) 
         (if (listp (second form))
	     (let ((temp (assoc ':CLASS (rest form))))
	       (cond ((and temp (listp (second temp)) (equal (first (second temp)) ':PRED))
		      (cons (first form) (rest (find-cl (second temp)))))
		     (temp
		      (cons (first form) (first (rest temp))))
		     (t 
		      (cons ':DESCRIPTION (first form)))))
	   NIL))
	(t NIL)))

;; This takes a semantic form and returns the constraints on it.

(defun find-edges (form)
  (let ((temp (first (rest (assoc ':CONSTRAINT (rest form))))))
    (if (equal (first temp) ':AND)
      (rest temp)
      (list temp))))


;;;  General functions

;; This acts like "subst", but if the "old" is not in the tree it inserts the 
;; "new" anyway.

(defun swap (new old tree &key (test #'equal))
  (if (not old) 
      (setf tree (append tree (list new)))
      (subst new old tree :test test)))

;; Average takes the average of a list of numbers.
||#

(defun average (list-of-nos)
  (let ((result 0))
    (dolist (element list-of-nos result)
      (setf result (+ element result)))
    (setf result (/ result (length list-of-nos)))))

;; Remove-till removes all the elements of a list up to and including the 
;; given element.

(defun remove-till (item list-of-items)
  (cond ((not list-of-items) nil)
	((equal item (first list-of-items))
	 (rest list-of-items))
	(t (remove-till item (rest list-of-items)))))

;; First-one returns which of thing1 and thing2 comes first in list, or nil
;; if neither are in list.

(defun first-one (thing1 thing2 list)
  (cond ((not list) nil)
	((equal thing1 (first list))
	 thing1)
	((equal thing2 (first list))
	 thing2)
	(t (first-one thing1 thing2 (rest list)))))

;; Swap-class takes a verb class and a speech-act and replaces the sa's
;; verb class with the given one.

#||
(defun swap-class (new act)
  (setf (SA-semantics act) 
    (swap (list ':CLASS new) (assoc ':CLASS (rest (SA-semantics act))) (SA-semantics act))))


;; Apply-fun is needed because of the crass stupidity of the LISP committee,
;; who for some very impractical reason determined that lambda functions 
;; will not satisfy the predicate functionp.  It applies the given function
;; to the listed arguments in the correct manner.

(defun apply-fun (fun &rest lastargs)
  (cond ((not (listp fun)) (apply fun lastargs))
	((equal (first fun) 'LAMBDA) (apply fun lastargs))
	(t (apply (first fun) (append (rest fun) lastargs)))))

;; assumes 'w' is a symbol
(defun dekeywordify (w)
  (cond ((numberp w) w)
	((symbolp w) (read-from-string (symbol-name w)))
	(t w)))


;; New-var is a hack.  It makes a new variable looking like the variables from 
;; the parser output.

(defun new-var ()
  (keywordify (gentemp 'V)))

;; we'll observe that a variable is something which is a symbol,
;; beginning with a colon, whose first char (otherwise) is V, and
;; whose subsequent characters comprise a number.
;;
;; we'll also accept :ELEM[\d]+
(defun var (x)
  (and (keywordp x)
       (or (and (eql #\V (elt (symbol-name x) 0))
		(numberp (read-from-string (subseq (symbol-name x) 1))))
	   (and (string= "ELEM" (subseq (symbol-name x) 0 4))
		(numberp (read-from-string (subseq (symbol-name x) 4)))))))


(defun varp (term)
  (and (keywordp term) (eql (char (string term) 0) #\V)))

;; This returns t only if all of the list are true of the object.  

(defun true (list object)
  (cond ((not list) t)
	((not (apply-fun (first list) object))
	 nil)
	(t (true (rest list) object))))

;; This finds the union of a list of sets.

(defun find-union (sets)
  (cond ((not sets) nil)
	((eq (length sets) 1) (first sets))
	(t (find-union (cons (union (car sets) (cadr sets)) (cddr sets))))))

(defun true-here (list object)
  (cond ((not list) nil)
	((not (apply-fun (first list) object))
	 nil)
	(t (true (rest list) object))))

(defun find-part (form part)
  (let* ((temp (find-edges form)) (obj (assoc part temp)))
    (if obj (third obj)
      nil)))

;; reads in from a file of the form:
;; (<SEXP>)
;; (<SEXP>)
;; so the file itself doesn't have the parens for the list
;; note the file is read in backwards to allow the tail recursion
(defun read-in-list (&optional (stream *standard-input*) (res nil))
  (let ((next (read stream nil 'eof)))
    (if (eq next 'eof)
	res
      (read-in-list stream (cons next res)))))

;; note this takes different args than 'read-in-list'.  just because
;; I don't like typing all the options to 'open' every time.
(defun write-out-list (l fname)
  (with-open-file (stream fname 
		   :direction :output
		   :if-exists :supersede
		   :if-does-not-exist :create)
    (mapcar #'(lambda (e)
		(format stream "~S~%" e))
	    l)))


;______________________________________________________________________________
;;;  Functions for use in the patterns.

;; This takes a new role (:LOBJ, :LCOMP, etc.), an old role, and a list of 
;; roles (e.g. the constraints on a speech-act).  It returns a new list of
;; roles with the new swapped for for the old.

(defun change-role (newrole oldrole roles)
  (if (assoc newrole roles)
      (swap 
       (swap newrole oldrole (assoc oldrole roles))
       (assoc newrole roles) 
       (remove (assoc oldrole roles) roles))
    (swap (swap newrole oldrole (assoc oldrole roles))
	  (assoc oldrole roles)
	  roles)))


;; This does change-role for a whole list of roles, and takes the union of
;; the result.

(defun change-roles (pairs act)
  (let ((roles (find-edges (SA-semantics act))) (result nil))
    (dolist (element pairs result)
      (push (change-role (first element) (second element) roles) result))
    (setf (SA-semantics act) (swap (find-union result) roles (SA-semantics act)))))

;; This replaces one item with another in a constraint, e.g. changes 
;; (:LCOMP :V1145 :V1156) to (:LCOMP :V1145 :V2267).

(defun replace-role (item role act)
  (let ((temp (find-edges (SA-semantics act))))
    (setf (SA-semantics act) 
      (swap (swap (append (butlast (assoc role temp)) (list item))
		  (assoc role temp)
		  temp) 
	    temp
	    (SA-semantics act)))))
||#

;;----------------------------------------------------------------------------





