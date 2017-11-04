;;;;
;;;; procedures.lisp
;;;;
;;;;
;;;;
;;;; This file contains some example procedural attachments
;;;;   Note - this fiule must preceed files that define grammars using
;;;;          these procedures
;;;;

(in-package :parser)

;;=========
;;  An example user defined procedure call

;; UNIFY - simply succeeds if its two arguments unify

(define-predicate 'W::UNIFY
    #'(lambda (args)
	(unify-args args))
  )

(define-predicate 'W::ROLE-UNIFY
    #'(lambda (args)
	(unify-role-args args))
  )

(defun check-bound-in-constraint (rolename constr) 
  "Verifies that this constraint binds the role name, i.e. is in the form (rolename ...)"
  ;; true if rolename is a subtype of the first one in constr
  (subtype 'w::role rolename (first constr)))
;;;  (let ((rname (strip-lf-form rolename ':*))
;;;	(crname (and (consp constr) (strip-lf-form (first constr) ':*)))
;;;	)
;;;    (and (consp constr)
;;;	 (subtype 'sem rname crname))
;;;       )1
;;;  )

(defun unify-role-args (args)
  "Unify (rolearg ?rolemap) ?rolesem and (roles ?roles). Checks for the case where ?rolemap is - and does not unify"
  (let* ((rolename (get-fvalue args 'W::arg0)) ;;(first args))
	 (rolesem (get-fvalue args 'W::arg1)) ;;(second args)))
	 (roles (get-fvalue args 'W::arg2)) ;;(third args)))
	 (constraint (get-fvalue args 'W::arg3)) ;;(fourth args)))
	 ;;(restr (third args))
	 (rolelist nil) (mrole nil)		
	 (rbound nil)
	 )
       ;; got myself an empty mapping - never mind unification, return t
    (cond
     ((eql rolename '-)
      *success*)     
     ((or (null roles) (eq roles '-) (var-p rolename))
      nil)     
     (t ;; otherwise things are really tricky.  
      ;; first we really have to see that the role is not bound somewhere in the constraint      
      (cond
       ((constit-p constraint)
	(setq rbound (find rolename (constit-feats constraint) :test #'check-bound-in-constraint)))
       ((consp constraint)
	(setq rbound (tree-find-if (lambda (x) (check-bound-in-constraint rolename x))
				   constraint)))
       )      
      (when (or (null rbound) (null (first rbound)))
	;; we are here only if we're sure that the role is not filled
	;; We start with matching
	;; a role name into role arguments, and replacing the parent
	;; with it, where applicable. If we succeed in it, then we do a
	;; regular unification
	
	(setq rolelist (constit-feats roles))
	;;(print (cons 'ABCDE rolelist))
	;;      (setf mrole (find rolename rolelist :test (lambda (x y) (subtypea  x (car y)))))
	(setf mrole (find-if #'(lambda (x) (subtypea rolename (car x))) rolelist))
	;;(setf mrole (find rolename rolelist))

	(let* ((mrolesem (or (second mrole) '-))
	       (res (grammar-match-vals mrolesem rolesem))
	       )
	  ;; (format t "mrolesem : ~S~%" mrolesem)
	  ;; Myrosia commented out. We used to replace the role name, but there's no need
	  ;; In reality, checking for being bound in the constraint replaces that
	  ;;	  (when (and res mrole)
	  ;;  (setf (car mrole) rolename))
	  res
	;;(print (cons 'ABCDEG rolelist))
	;;	(grammar-match-vals rolesem (second mrole))
	  
	)
	)
      ))
     ))

(defvar *exact-match-roles* '(ont::predicate))

(defun subtypea (x y)
  (cond
   ((or (member x *exact-match-roles*) (member y *exact-match-roles*))
    (eql x y)
    )
   (t
    (subtype 'w::role x y)
    )
   ))

(defun grammar-match-vals (v1 v2)
;  (trace match-vals1 compat feature-intersect unify-sems unify-sem-arrays)
;  (setq FAILURE-TRACE t)
;   (let ((res
  (match-vals 'w::role v1 v2)
;;;	 ))
;;;    (untrace match-vals1 compat feature-intersect unify-sems unify-sem-arrays)
;;;  (setq FAILURE-TRACE nil)
;;;    res)
  )

(defun unify-args (args)
  ;;(let ((arg0 (get-fvalue args :arg0))) ;; (second (first args))))
    (match-vals 'w::sem (get-fvalue args 'w::pattern) (get-fvalue args 'w::value))
;;		arg0 
    )

(define-predicate 'w::GT
    #'(lambda (args)
	(let ((arg1 (get-fvalue args 'w::arg1))
	      (arg2  (get-fvalue args 'w::arg2)))
	(if (and (numberp arg1)
		 (numberp arg2)
		 (> arg1 arg2))
	    *success*))))

(define-predicate 'w::BOUND
  #'(lambda (args)
      (check-if-bound (get-fvalue args 'w::arg1))))

(define-predicate 'w::recompute-spec
    #'(lambda (args)
	(recompute-spec args)))

(defun recompute-spec (args)
  (let ((spec (get-fvalue args 'w::spec))
	(agr  (get-fvalue args 'w::agr))
	(result (get-fvalue args 'w::result)))
    
    (if (member spec '(ONT::DEFINITE W::DEFINITE))
	(if (equal agr 'w::|3P|)
	    (match-vals nil result 'ONT::DEFINITE-PLURAL)
	    (match-vals nil result 'ONT::DEFINITE))
	(if (member  spec '(ONT::INDEFINITE W::INDEFINITE))
	    (if (equal agr 'w::|3P|)
		(match-vals nil result 'ONT::INDEFINITE-PLURAL)
		(match-vals nil result 'ONT::INDEFINITE))
	    (match-vals nil result spec))
   )))

(defun check-if-bound (var)
  "succeeds only if arg is bound to something not equal to -"
  (if (var-p var) 
    (and (var-values var) (not (eq (var-values var) '-)) *success*)
    *success*))

(define-predicate 'w::NOT-UNIFY
  #'(lambda (args)
      (check-not-unify (get-fvalue args 'w::arg1) (get-fvalue args 'w::arg2))));;(second (first args)) (second (second args)))))

(defun check-not-unify (x y)
  (and (not (match-vals nil  x y)) *success*))

(define-predicate 'w::Add-to-conjunct
  #'(lambda (args)
        (add-new-constraint args)))

(defun add-new-constraint (args)
  "inserts a constraint into a new constraint structure and binds the third arg to it"
  (let ((newval (second (assoc 'w::val args)))
        (oldconstraint (second (assoc 'w::old args)))
        (newconstraintvar (second (assoc 'w::new args))))
    (cond
     ((and (constit-p newval) (eql (constit-cat newval) '&))
      ;; adding a complex constraint
      (add-features-to-constraint (constit-feats newval) oldconstraint newconstraintvar))
     ((consp newval) ;; we assume that our constraint is just 1 feature
      (add-features-to-constraint (list newval) oldconstraint newconstraintvar))
     ((null newval)
      (match-vals nil newconstraintvar oldconstraint))
     (t
      ;; illegal value, print warning and fail
      (parser-warn "Attempt to add an illegal constraint ~S. ~%   Constraints must be lists or & constituents" newval))))
  )

(define-predicate 'w::compute-sem-features
  #'(lambda (args)
      (compute-sem args)))

(defun compute-sem (args)
  (let* ((lf (second (assoc 'w::lf args)))
	 (semresult (second (assoc 'w::sem args)))
	 (domain-info (second (assoc 'w::domain-info args)))
	 (initial-sem (lxm::get-sem-for-lf (get-core-type lf))))
    (if domain-info 
	(setq initial-sem initial-sem
	      ))
    (match-vals nil semresult (read-expression (lxm::get-sem-for-lf (get-core-type lf))))))

(defun get-core-type (x)
  (if (consp x)
      (second x) 
      x))

(define-predicate 'w::compute-ont-type-from-sem
     #'(lambda (args)
      (compute-lf-from-sem args)))

  
(defun compute-lf-from-sem (args)
  (let* ((sem (second (assoc 'w::sem args)))
	 (lf (second (assoc 'w::lf args))))
    (match-vals nil lf (or (;; finish when new SEM strctures are installed
			    )))))

(defun add-features-to-constraint (feats oldconstraint newconstraintvar)
  "A helper to add-new-constraint. Given a list of features and an old constraint, creates a new constraint with features added to the old constraint. "
  (cond
   ((constit-p oldconstraint)
    (match-vals nil newconstraintvar 
		(make-constit :cat '& 
			      :feats (append feats
					     (remove-if #'(lambda (c) (eq (car c) '-))
							(constit-feats oldconstraint))))))
   ;;  oldconstraint is not specified
   ((or (null oldconstraint) (eq oldconstraint '-) (var-p oldconstraint))
    (match-vals nil newconstraintvar (make-constit :cat '& :feats feats))
    )))
  
  
(define-predicate 'w::Append-conjuncts
  #'(lambda (args)
        (append-conjuncts args)))

(defun append-conjuncts (args)
  "inserts a constraint into a new constraint structure and binds the third arg to it"
  (let ((conj1 (second (assoc 'w::conj1 args)))
        (conj2 (second (assoc 'w::conj2 args)))
        (newconjunctvar (second (assoc 'w::new args))))
    (if (null newconjunctvar)
      (;;parser-warn 
       break "Warning no value defined for result in call to APPEND-CONJUNCTS: Args are ~S" args))
    (cond
     ((and (constit-p conj1) (constit-p conj2) (eq (constit-cat conj1) '&) (eq (constit-cat conj2) '&))
      (match-vals nil newconjunctvar 
                  (make-constit :cat '& 
                                :feats (append (constit-feats conj1) 
                                               (remove-if #'(lambda (c) (eq (car c) '-))
                                                          (constit-feats conj2))))))
     ((or (null conj1) (eq conj1 '-) (var-p conj1))
      ;; CONJ1 is empty, so we simply bind to CONJ2 if its set
           (if (and (constit-p conj2) (eq (constit-cat conj2) '&))
	       (match-vals nil newconjunctvar conj2)
	     *success*))
     ((or (null conj2) (eq conj2 '-) (var-p conj2))
      (if (and (constit-p conj1) (eq (constit-cat conj1) '&))
	  (match-vals nil newconjunctvar conj1)
	*success*))
     (t  (parser-warn "Ill-formed conjunct found in call to APPEND-CONJUNCTS: ~S" args)
         nil))
   
    ))

(define-predicate 'w::change-feature-values
  #'(lambda (args)
        (change-feature-values args)))

(defun change-feature-values (args)
  "takes a feature structure and a list of feature values and replaces/sets those features
   args are ((OLD <old-feature-structure>) (NEWVALUES (<Feat-value-pair*>)) (NEW <variable for result>))"
  (let ((new (second (assoc 'w::new args)))
        (old (second (assoc 'w::old args)))
        (fvs (second (assoc 'w::newvalues args))))
    (if (or (null fvs) (null old))
      (parser-warn "~%Warning no ~S defined in call to CHANGE-FEATURE-VALUE: ~%Args were ~S" 
                   (if (null old) "old feature set (i.e., OLD)" 
                       "feature values (i.e., NEWVALUES)" )
                    args))
    (if (and (not (eql fvs 'w::*NOCHANGE*)) (or (not (consp fvs)) (not (every #'consp fvs))))
      (parser-warn "~%Warning bad call to CHANGE-FEATURE-VALUE: NEWVALUES must be a list of feat-val pairs. ~%Args were ~S" 
                    args))
    (let 
      ((newfeats 
	(cond
	 ;;  a SEM structure
	 ((and (var-p old) (arrayp (var-values old)))
	  (let* 
	      ((copy (copy-sem-array (var-values old))))
	    (when (not (eql fvs 'W::*NOCHANGE*))
	      (set-feature-values-in-array fvs copy)
	      )
	    (make-var :name 'w::SEM :values copy)))
	 ;; a var with a single constit value
	 ((and (var-p old) (constit-p (var-values old)))
	  (make-var :name (gen-symbol 'var)
		    :values (replace-feature-values-in-constit (var-values old) fvs)))
	 ;; a var with a list of values
	 ((var-p old)
	  (make-var :name (gen-symbol 'var)
		    :values (mapcar (lambda (x)
				      (if (constit-p x)
					  (replace-feature-values-in-constit x fvs)
					x))
				    (var-values old))))
	 ((constit-p old)
	  (replace-feature-values-in-constit old fvs))
	 )))
      (match-vals nil new newfeats))
    ))
        

(defun copy-sem-array (arr)
  (let ((new (make-array *sem-size* :initial-element *default-sem-variable*)))
    (dotimes (i *sem-size*)
      (setf (aref new i) (aref arr i)))
    new))

(defun replace-feature-values-in-constit (c fvs)
  (if (null fvs) 
    c
    ;;    (replace-features-in-constit (replace-feature-value c (caar fvs) (cadar fvs)) (cdr fvs))))
    (replace-feature-values-in-constit (replace-feature-value c (caar fvs) (cadar fvs)) (cdr fvs))))


  
(define-predicate 'w::sem-least-upper-bound
  #'(lambda (args)
      (sem-lub args)))

(defun sem-lub (args) 
 (let ((in1 (second (assoc 'w::in1 args)))
       (in2 (second (assoc 'w::in2 args)))
       (OUT (second (assoc 'w::out args))))
   (if (and (var-p in1) (var-p in2) out)
       (let ((result (compute-sem-lub in1 In2)))
	 (match-vals nil out
		     (make-var :name (gen-symbol 'sem) :values result)))
       *success*
	)))

(define-predicate 'w::class-greatest-lower-bound
  #'(lambda (args)
      (class-cglb args))
  )

(defun class-cglb (args) 
  (let* ((in1 (second (assoc 'w::in1 args)))
        (in2 (second (assoc 'w::in2 args)))
	(OUT (second (assoc 'w::out args)))
	(cglb (compute-class-cglb in1 in2)))
    (if cglb
	(match-vals nil out cglb)
	(values (match-vals nil out in2) .9))))

(defun compute-class-CGLB (in1 in2)
  (cond
   ((var-p in1)
    in2)
   ((var-p in2)
    in1)
   ((subtype 'w::sem in1 in2)
    in1)
   ((subtype 'w::sem in2 in1)
    in2)
   ))


(define-predicate 'w::class-least-upper-bound
  #'(lambda (args)
      (class-lub args)))

(defun class-lub (args) 
 (let* ((in1 (second (assoc 'w::in1 args)))
	(in2 (second (assoc 'w::in2 args)))
	(OUT (second (assoc 'w::out args)))
	(lub (compute-class-lub in1 in2))
	(core-in1 (core-type in1))
	(core-in2 (core-type in2))
	(parents1 (om::get-parents core-in1))
	(parents2 (om::get-parents core-in2)))
   (values 
    (match-vals nil out lub)
    (cond ((member lub '(ont::referential-sem ont::root)) .9)
	  ((equal core-in1 core-in2) 1)  
	  ((and parents1 parents2) (+ 0.96 (/ (* 0.04 0.9) (+ 1 (max (or (position lub parents1) 0)
								     (or (position lub parents2) 0))))))
	  (t .98)))))

(defun core-type (x)
  (if (consp x) (second x)
      x))

(defun compute-class-lub (in1 in2)
  (cond
   ((subtype 'w::sem in1 in2)
    in2)
   ((subtype 'w::sem in2 in1)
    in1)
   ;; catch the simple case of two specialized types
   ((and (consp in1) (consp in2))
    (compute-class-lub (second in1) (second in2)))
   ((var-p in1)
    (cond
     ((var-p in2)
      (if (and (consp (var-values in1)) (consp (var-values in2)))
	  (make-var :name (gen-symbol 'v)
		    :values (union (var-values in1) (var-values in2)))
	'ont::ROOT))
     ((or (consp in2) (symbolp in2))
      (if (consp (var-values in1))
	  (cond
	   ((member in2 (var-values in1))
	    in1)
	   (t (make-var :name (gen-symbol 'v) :values (cons in2 in1))))
	in2)) ;; Not sure if this is right, in1 is an unbound variable, so whats the LUB?
     (t 'ont::ROOT)))
   ((var-p in2)
    (if (and (consp (var-values in2))
	     (member in1 (var-values in2)))
	in2
      'ont::ROOT))
   (t 
    (or (om::common-ancestor (if (consp in1) (second in1) in1)
			     (if (consp in2) (second in2) in2))
	  'ont::ROOT))))

(define-predicate 'w::simple-cons
  #'(lambda (args)
      (simple-cons args)))

(defun simple-cons (args)
  (let ((in1 (second (assoc 'w::in1 args)))
        (in2 (second (assoc 'w::in2 args)))
	(OUT (second (assoc 'w::out args))))
    (if (and (symbolp in1) (listp in2))
      (match-vals nil out
		  (cons in1 in2))
      (progn
	(format t "~%Failed trying to cons ~S to ~S" out (cons in1 in2))
	nil))))
		
(define-predicate 'W::substitute-vars
  #'(lambda (args)
      (subst-var args)))

(defun subst-var (args)
  (let
      ((oldv (second (assoc 'w::oldv args)))
       (newv (second (assoc 'w::newv args)))
       (old-lf (second (assoc 'W::old-lf args)))
       (new-lf (second (assoc 'W::new-lf args))))
    (match-vals nil new-lf
		(subst newv oldv old-lf))))

;; Number Handling
;;    we need to do arithmatic here and then compute the new NTYPE so we can enforce rules on dates, etc.

       
(define-predicate 'W::compute-val-and-ntype
  #'(lambda (args)
      (process-number args)))

(defun process-number (args)
  (let
      ((expr (second (assoc 'w::expr args)))
       (newval (second (assoc 'W::newval args)))
       (ntype (second (assoc 'W::ntype args))))
    
    (let ((result (add-em-up expr)))
      (if result
	  (append (match-vals nil newval result)
		  (if (numberp result) (match-vals nil ntype (classify-n result))))
	
	(parser-warn "Bad number Value in args to NUMBER ~S:" args))
    )))

;; 05/14/00 Myrosia added the "twodigit" classification for all numbers smaller than 100
;;   4/02 Added zero classification - zero isn't a digit! (e.., *twenty zero) JFA
(defun classify-n (n)
  ;;  classifies numbers in ranges for clock times, etc.
  (let ((vals
	 (cond 
	  ((not (eql (rem n 1) 0)) ;; check for non-integers
	   (if (< n 0) 
	       '(w::negative w::decpoint)
	     '(w::positive w::decpoint)))
	  ((< n 0) '(w::negative))
	  ((<= n 9) '(w::digit w::hour12 w::day)) ;;; removed w::minute since single digits don't work in time expressions
	  ((<= n 12) '(w::twodigit w::teen w::hour12 w::minute w::day))
	  ((<= n 19) '(w::twodigit w::teen w::minute w::day))
	  ((= n 20) '(w::twodigit w::tens w::hour24 w::minute w::day))
	  ((<= n 23) '(w::twodigit w::hour24 w::minute w::day))
	  ((= n 30) '(w::twodigit w::tens w::minute w::day))
	  ((<= n 31) '(w::twodigit w::minute w::day))
	  ((= n 40) '(w::twodigit w::tens w::minute))
	  ((= n 50) '(w::twodigit w::tens w::minute))
	  ((< n 60) '(w::twodigit w::minute))
	  ((= n 60) '(w::twodigit w::tens))
	  ((= n 70) '(w::twodigit w::tens))
	  ((= n 80) '(w::twodigit w::tens))
	  ((= n 90) '(w::twodigit w::tens))
	  ((< n 100) '(w::twodigit))
	  ((and (>= n 100) (< n 1000)) '(w::threedigit))
	  ((and (> n 1600) (< n 2100)) '(w::year))
	  (t '-))))
    (if (eq vals '-)
	'-
      (make-var :name (gen-symbol 'n) :values vals))))


(defun numeric-expression (x)
  (or (numberp x)
      (and (consp x) (member (car x) '(W::times* W::decimal-point w::fraction)))))
 
;; changed that to allow multiplication and nested addition
(defun add-em-up (x)
  (if (numberp x) x
      (if (consp x)
	  (cond
	    ((eq (car x) 'w::decimal-point)
	     (calculate-fraction-value (add-em-up (second x)) (add-em-up (third x))))
	    ((eq (car x) 'w::fraction)
	     (calculate-fraction (add-em-up (second x)) (add-em-up (third x))))
	    ((eq (car x) '+)
	     (+ (add-em-up (second x)) (add-em-up (third x))))
	    ((eq (car x) 'w::times*)
	     (* (add-em-up (second x)) (add-em-up (third x))))
	    ((eq (car x) '-)
	     (- (add-em-up (second x)) (add-em-up (third x))))
	    ((eq (car x) 'w::/)
	     (/ (add-em-up (second x)) (add-em-up (third x))))
	    ((eq (car x) 'w::stringappend) ;;  append two numbers together that were separated by commas e.g., 17,343
	     (format nil "~A~A" (second x) (third x)))
	    ((eq (car x) 'w::string-to-number)
	     (safe-read-from-string (second x)))
	    (t 0)
	    )
	  0)))


(defun calculate-fraction-value (n1 n2)
  "Given n1 and n2, return the value n1.n2, taking into account that n1 may be positive or negative"
  ;; convert n2 into a fractional value
  ;;(loop while (>= n2 1) do (setq n2 (* n2 0.1)))   -- this gets us into trouble with floating poitn arithmetic!
  (setq n2 (read-from-string (format nil ".~A" n2)))
  (if (>= n1 0)
      (+ n1 n2)
      (- n1 n2)
    )
  )

(defun calculate-fraction (n1 n2)
  "Given n1 and n2, return the value n1/n2, taking into account that n1 may be positive or negative"
  (/ n1 n2)
  )

(define-predicate 'W::less-than
  #'(lambda (args)
      (less-than-check args)))

(defun less-than-check (args)
  (let
      ((v1 (second (assoc 'w::val1 args)))
       (v2 (second (assoc 'W::val2 args)))
       )
    (if (and (numberp v1) (numberp v2) (< v1 v2))
	*success*
      )))
     
(define-predicate `W::add-to-end-of-list
  #'(lambda (args)
      (append-build args)))

(defun append-build (args)
  (let
      ((list (second (assoc 'w::list args)))
       (val (second (assoc 'W::val args)))
       (newlist (second (assoc 'W::newlist args))))
    (if (eq list '-) (setq list nil))
    (if (listp list)
	(match-vals nil newlist (append list (list val)))
    )))

(define-predicate 'W::compute-complex
    #'(lambda (args)
	(compute-complex args))
  )

(defun compute-complex (args)
   (let ((subcatvar (cadr (car args)))
	 (complex (cadr (second args))))
     ;;(format t "~%RESTR is ~S" subcatvar)
     (match-vals nil complex (if (var-p subcatvar) '- '+))))


(define-predicate 'w::logical-and
    #'(lambda (args)
	(logical-and args))
  )

(defun logical-and (args)
  (let* ((arg1 (second (assoc 'w::in1 args)))
	 (arg2 (second (assoc 'w::in2  args)))
	 (out (second (assoc 'w::out args)))
	 (result	 
	  (cond
	   ((or (not (member arg1 '(+ -)))
		(not (member arg2 '(+ -)))
		)
	    nil)
	   ((or (eql arg1 '-) (eql arg2 '-))
	    '-)
	   (t '+)
	   )
	  )
	 )
    (match-vals nil out result)
    ))

(define-predicate 'w::combine-status
    #'(lambda (args)
	(combine-status args))
  )

(defun combine-status (args)
  (let ((in1 (second (assoc 'w::in1 args)))
        (in2 (second (assoc 'w::in2 args)))
	(out (second (assoc 'w::out args)))
	)
    (if (and (member in1 '(ONT::DEFINITE ONT::DEFINITE-PLURAL))
	     (member in2 '(ONT::DEFINITE ONT::DEFINITE-PLURAL)))
	(match-vals nil out 'ONT::DEFINITE-PLURAL)
        (match-vals nil out 'ONT::INDEFINITE-PLURAL)
      )
))

