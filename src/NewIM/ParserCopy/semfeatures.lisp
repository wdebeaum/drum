;; THIS IS AN EXACT COPY OF sem-features.lisp

(in-package "IM")


;;
;;  SEM sructure manipulation
;;
;;  SEM structures are arrays to support fast feature unification
;;
;;  Builds a hash table that maps features to array indices. The common features are
;;    early on in the array, and then individual features share the remaining values
;;    based on the type.  e.g.,
;;    common-features (X Y); indiv-features (type1 z) (type2 a b)) would be encode as 
;;    an array of length 5. A<1> is the type (i.e., type1 or type2); a<1> and a<2> are
;;    the values for the common features X and Y, a<3> is the value of Z for type1, and A for
;;    type2, and a<4> is unused for type1 and the value for B for type2"

;; Note - during unification, we will always build a new SEM array for the result (unless 
;;   one value is not changed). So we can reuse the *deafult-sem-array* whenever we want
;;   without risk of unwanted connections. Also, to reduce consing,
;;   we make a special check for the *default-sem-variable*
;;   and treat it as a wildcard not related to other instances of *default-sem-variable*.

(defvar *default-sem-variable* (make-var :name 'DS))

(defvar *default-sem-array* nil)

(defvar *sem-types* nil)
(defvar *sem-features*)
(defvar *sem-size* nil)
(defvar *feature-to-index*)
(defvar *index-to-feature*)

(defun get-sem-features nil
  *sem-features*)

(defun compile-sem-features (types common-features indiv-features)
  "Build the tables to drive SEM array building and unification:
    TYPES: the set of sem types allowed; common-features - features shared by all types;
    INDIV-FEATURES - a list, of each type, of the individual features"
  (let* ((max-indiv-size 
	  (if (not (null indiv-features))
	      (apply #'max (mapcar #'list-length indiv-features))
	      0))
         (indiv-feat-list (if (not (null indiv-features))
			      (reduce #'append (mapcar #'cdr indiv-features))
			      ))
         (common-size (list-length common-features))
         (start-indiv-count (+ common-size 1))
	 )
    (if (intersection common-features indiv-feat-list)
      (parser-warn "SEM feature(s) declared both as common and individual: ~S"
                   (intersection common-features indiv-feat-list)))
    (if (not (eql (list-length (remove-duplicates indiv-feat-list))
		  (list-length indiv-feat-list)))
      (parser-warn "There are one or more SEM features defined in more than one individual list should be declared as common: ~S"
                   indiv-features))
    (setq *sem-types* types)
    (setq *sem-features* (append common-features indiv-feat-list))
    (setq *sem-size* (+ common-size Max-indiv-size 1))
    (setq *feature-to-index* (make-hash-table :size *sem-size*))
    (setq *index-to-feature* (make-hash-table))
    (setf (gethash 'common *index-to-feature*) (make-array *sem-size* :initial-element nil))			
    (init-feature-to-index common-features 1 :type 'common)
    (mapcar #'(lambda (x)
		(setf (gethash x *index-to-feature*) (make-array *sem-size* :initial-element nil)))			
                ;;(init-feature-to-index (cdr x) start-indiv-count :type (car x)))
            types)
    (setq *default-sem-array* nil) ;;(build-sem-array nil nil))
    (values)))

(defun init-feature-to-index (feats index &key (type nil))
  "Builds hash table to map features names to array indices"
  (when feats
    (setf (gethash (car feats) *feature-to-index*) index)
    (setf (aref (gethash type *index-to-feature*) index) (car feats))      
    (init-feature-to-index (cdr feats) (+ index 1) :type type)
    )
  )


;;
;;  Constructing SEM Arrays from a SEM constit build by the reader.
;;
;;  SEM structures are lists beginning with $

(defun is-sem-structure (v)
  (and (consp v)
       (equal (util::convert-to-package (car v) :parser) '$)))

(defun build-sem-array (expr rule-id)
  "Given a constit that specifies a SEM feature value, build the SEM array"
  (if
    (and (consp expr) (eq (car expr) '$))
    (let ((arr (make-array *sem-size* :initial-element *default-sem-variable*)))
      (setf (aref arr 0) (second expr))
      (set-feature-values-in-array (cddr expr) arr)
      arr)
    (progn
      (parser-warn "Build-sem-array: Illegal SEM value: ~S in rule ~S is list? ~S" expr rule-id (consp expr))
      expr)))

(defun set-feature-values-in-array (feats arr)
  "Sets the specified values in the array"
  (when feats
    (let ((index (gethash (caar feats) *feature-to-index*)))
      (if index
        (Progn
          (setf (aref arr index)
                (second (car feats)))
          (set-feature-values-in-array (cdr feats) arr))
        (parser-warn "Undeclared semantic feature ~S in ~S" (caar feats) feats)))))

(defun get-sem-feature-value (sem feat)
  "returns the FEAT feature value in the sem array SEM"
  (let ((index (gethash feat *feature-to-index*)))
    (if index (aref sem index))))
    
(defun build-sem-variable (constit rule-id)
  "Given a constit, build a sem variable"
  ;;(break)
  (make-var :name (gen-v-num 'sem) ;;(gen-symbol 'sem) 
	    :values (build-sem-array constit rule-id)))
      

;; printing SEM arrays

(defun show-sem-array (arr)
  (format t "[")
  (dotimes (i *sem-size*)
    (let ((v (aref arr i)))
      (cond
       ((null v) (format t "-"))
       ((eq v *default-sem-variable*)
        (format t "?"))
       (t (format t " ~S " v)))))
  (format t "]"))

;; This is a top-level function for unifying to sem structures
(defun unify-sem-structures (sem1 sem2)
  (multiple-value-bind (result bindings score undos)
      (unify-sems sem1 sem2 nil)
    (undo-bindings undos)
    (values result bindings score)))

;; unifying SEM arrays
;;   note: not a top level function
(defun unify-sems (sem1 sem2 undos)
  "unifies two semantic feature vectors and returns the result and a probability reflecting the goodness of match (if flexible-semantic-matching is turned on)"
  (if (and (var-p sem1) (var-p sem2))
    (let ((arr1 (var-values sem1))
          (arr2 (var-values sem2)))
      (cond
	((null arr1)
	 (values sem2 nil 1 undos))
	((null arr2)
	 (values sem1 nil 1 undos))
	((arrayp arr1)
	 (cond
	   ;; usual case, two arrays
	   ((arrayp arr2)
	    (if (ident-sem-array arr1 arr2)  ;; if they are ground and identical, we don't need a new copy
		(values sem1 nil 1 undos)
		(multiple-value-bind (ans bndgs prob newundos)
		    (unify-sem-arrays arr1 arr2 undos)
		  (if (null ans) 
		      (values nil nil nil newundos)
		      (values (make-var :name (gen-v-num 'sem) ;;(gen-symbol 'sem) 
					:values ans) 
			      bndgs prob newundos)))))
	   (t
	    (values nil nil nil undos))
	    ;; not handled yet 
	   ))
	;;  arr2 is an array, arr1 should be a list
	((and (arrayp arr2) (consp arr1)) 
	 (values nil nil nil undos))))
     (values nil nil nil undos))
  )

(defun ident-sem-array (a1 a2)
  (let ((OK t))
    (do ((i 0 (+ i 1))) ((or (>= i *sem-size*) (not OK)))
      (let ((v1 (aref a1 i))
            (v2 (aref a2 i)))
	(if (not (eq v1 v2))
	    (setq OK nil))))
    OK))
      
(defun unify-sem-arrays (s1 s2 undos)
  "Unifies two sem arrays and returns two arguments: the result and any new variable bindings"
  (let
    ((s1-modified nil)
     (s2-modified nil)
     (result (make-array *sem-size* :initial-element *default-sem-variable*))
     (accumulated-bndgs nil)
     (fail nil)
     (accumulated-prob 1)
     (newundos undos))
    ;;  Check each array value unless we fail
    (do ((i 0 (+ i 1))) ((or (>= i *sem-size*) fail))
      (let ((v1 (aref s1 i))
            (v2 (aref s2 i)))
        (if (var-p v1)
          (setq v1 (chase-down-var v1)))
        (if (var-p v2)
          (setq v2 (chase-down-var v2)))
        (cond
         ((eq v1 v2)
          (setf (aref result i) v1))
         ((eq v1 *default-sem-variable*)
          (setf (aref result i) v2)
          (setq s1-modified t))
         ((eq v2 *default-sem-variable*)
          (setf (aref result i) v1)
          (setq s2-modified t))
         ;; special case of two symbols - pick the most specific
         ((and (symbolp v1) (symbolp v2))	  
          (cond
            ((subtype 'w::sem v1 v2)
             (setf (aref result i) v1)
             (setq s2-modified t))
            ((subtype 'w::sem v2 v1)
             (setf (aref result i) v2)
             (setq s1-modified t))
            (t 
	     (if (flexible-semantic-matching *chart*)
		 (progn
		   (setq s1-modified t) (setq s2-modified t)
		   (trace-msg 1 "~%Semantic feature violation found: values ~S and ~S" v1 v2)
		   (if (not (eql i 1))
		       (Setq accumulated-prob (* accumulated-prob (compute-sem-failure-penalty i v1 v2)))
		       (trace-msg 1 "~%No penalty for the KR-TYPE feature in ~S and ~S" v1 v2)))
		 (setq fail t)))))
         ;; general case, try unifying the standard way
         (t
	  (multiple-value-bind (bndgs prob newundos1)
	      (match-vals1 'w::sem v1 v2 newundos)
	    (setq newundos newundos1)
	    (if bndgs
		(progn
		  (when (not (or (null prob) (= prob 1)))
		      (setq accumulated-prob (* accumulated-prob prob)))
		  (Setq Accumulated-bndgs (add-to-binding-list bndgs accumulated-bndgs))
		  (let* ((newv1 v1) ;;(subst-in v1 bndgs))  since we're in the middle of unificaiton, why would Subst be necessary
			 (newv2 v2) ;;(subst-in v2 bndgs))
			 (newv (if (subtype 'w::sem newv1 newv2) newv1 newv2)))
		    (cond ((equal newv v1)
			   (setf (aref result i) v1)
			   (setq s2-modified t))
			  ((equal newv v2)
			   (setf (aref result i) v2)
			   (setq s1-modified t))
			  (t
			   (setf (aref result i) newv)
			   (setq s1-modified t)
			   (setq s2-modified t)))))
		(if (flexible-semantic-matching *chart*)
		 (progn
		   (setq s1-modified t) (setq s2-modified t)
		   (if (not (eql i 1))
		     (progn
		       (trace-msg 1 "~%Semantic feature violation found")
		       (Setq accumulated-prob (* accumulated-prob (compute-sem-failure-penalty i v1 v2))))
		      (trace-msg 1 "~%No penalty for the KR-TYPE feature in ~S and ~S" v1 v2)))
		 (setq fail t)))))
	 )))
    ;;(format t "~%Failed?: ~S:" fail)
    ;; build value to return
    (if (not fail)
      (cond 
       ((not s1-modified)
        (values s1 accumulated-bndgs accumulated-prob newundos))
       ((not s2-modified)
        (values s2 accumulated-bndgs accumulated-prob newundos))
       (t 
        (values result accumulated-bndgs accumulated-prob newundos)))
      (values nil nil nil newundos))))

(defun compute-sem-failure-penalty (index v1 v2)
  (if (= index 0)
      ;; we failed to match at the top level semantic feature
      (if (or (eq v1 'F::TIME) (eq v2 'F::TIME))
	  (* *sem-failure-penalty* *sem-failure-penalty* *sem-failure-penalty*)  ;; severe penalty for F::TIME mismatch
	  (* *sem-failure-penalty* *sem-failure-penalty*))
      *Sem-failure-penalty*))

(defun get-feature-name-by-index (type index)
  (OR (and (gethash type *index-to-feature*) (aref (gethash type *index-to-feature*) index))
      (and (gethash 'common *index-to-feature*) (aref (gethash 'common *index-to-feature*) index))
      'unknown)
  )

(defun compute-sem-lub (sem1 sem2)
  "Computes an approximate LUB of two sem arrays by computing the LUB in each dimension, returns two value: new SEM and any variable bindings"
  (if (and (var-p sem1) (var-p sem2))
      (let
	  ((s1 (var-values sem1))
	   (s2 (var-values sem2))
	   )
	(cond
	  ((and (arrayp s1) (arrayp s2))
	   ;;  Check each array value 
	   (let
	       ((result (make-array *sem-size* :initial-element *default-sem-variable*))
		(type1 (aref s1 0))
		(type2 (aref s2 0))
		)
	     ;; a special case for 0, which is the type
	     (cond
	       ((or (eq type1 type2) (var-p type2) (var-p type1))
		(setf (aref result 0) (if (var-p type2) type1 type2))
		(do ((i 1 (+ i 1))) ((>= i *sem-size*))
		  (setf (aref result i)
			(compute-lub-value (aref s1 i) (aref s2 i)))))
	       (t
		(setf (aref result 0)
		      (make-var :name (gen-v-num 'v) ;;(gen-symbol 'v) 
				:values (union (var-or-symbol-values type1) (var-or-symbol-values type2))))))
	    result))
       ;; they are not SEM arrays
       ((null s1)
	(make-var :name (gen-v-num 'v) ;;(gen-symbol 'v) 
		  :values s2))
       ((null s2)
	(make-var :name (gen-v-num 'v) ;(gen-symbol 'v) 
		  :values s1))
       ((and (consp s1) (consp s2))
	(make-var :name (gen-v-num 'v) ;(gen-symbol 'v) 
		  :values (union s1 s2)))
      ;; not sure what would ever end up here
       (t
	(make-var :name (gen-v-num 'v))))) ;(gen-symbol 'v)))))
    (make-var :name (gen-v-num 'v)))) ;(gen-symbol 'v))))



(defun var-or-symbol-values (v)
  "If v is a variable, returns the values as a list, if it is a symbol, returns it as a 1-element list"
  (if (and (var-p v) (consp (var-values v)))
      (var-values v)
      (list v)))
  

(defun compute-lub-value (v1 v2)
  "Compute the least upper bound for the values v1 and v2"
  (if (var-p v1)
      (setq v1 (chase-down-var v1)))
  (if (var-p v2)
      (setq v2 (chase-down-var v2)))
  (cond
   ((eq v1 v2)
    v1)
   ((eq v1 *default-sem-variable*)
    v2)
   ((eq v2 *default-sem-variable*)
    v1)
   ;; special case of two symbols - find the LUB (i.e., opposite of unifying, we take the more general
   ;;     and if neither is a subtype of the other, we return a variable 
   ((and (symbolp v1) (symbolp v2))	  
    ;; Myrosia 2003/10/02 temporarily changed to "or" - this is a better bet for now
    ;;    (or (more-specific-feature-value v1 v2) 'inconsistent))
    (make-var :name (gen-v-num 's) ;(gen-symbol 's) 
	      :values (list v1 v2)))
   ;; cases with variables
   ((and (var-p v1) (null (var-values v1)))
    ;;    (make-var :name (gen-symbol 's) :values (var-values v2)))
    v2)
   ((and (var-p v2) (null (var-values v2)))
    ;;(print "null v2")			
    ;; (make-var :name (gen-symbol 's) :values (var-values v1)))
    v1)
   ;; if we have variables here, their values are non-null
   ((or (var-p v1) (var-p v2))
    ;;(print "var v1")			
    (let ((vals (lub-union (if (var-p v1) (var-values v1) v1)
			    (if (var-p v2) (var-values v2) v2))))
      (if vals
	  (make-var :name (gen-v-num 's) ;(gen-symbol 's) 
		    :values vals)
	  'inconsistent))						    
    )
   ;; is v1 and v2 are unifiable, return them
   ((lub-union v1 v2))
   (t ;; v1 and v2 are not unifiable
    'inconsistent))
  )
  

  
(defun more-specific-feature-value (v1 v2)
  "Given 2 values, returns more-specific, or nil if they don't unify"
  (or (subtype 'w::sem v1 v2) (subtype 'w::sem v2 v1)))

(defun find-most-specific-feature-value (v1 l)
  "Given a list, if v1 is a subtype of something in the list, return v1. If the list contains a subtype of v1, return it. Otherwise, return nil"
  (let* ((newval (if (symbolp l) (list l) l))
	 (res (find v1 newval :test #'more-specific-feature-value)))
    (when res
      (more-specific-feature-value v1 res))))
		  

(defun lub-union (l1 l2)
  "Given 2 lists, return only the most specific elements contained in both lists"
  (cond ((symbolp l1)
	 (if (symbolp l2)
	       (more-specific-feature-value l1 l2)
	     (find-most-specific-feature-value l1 l2)))
	  ((consp l1)
	   (let ((result nil)
		 (newl2 (if (symbolp l2) (list l2) l2)))
	     (dolist (el l1)
	       (let ((nel (find-most-specific-feature-value el newl2)))
		 (when nel
		   (pushnew nel result))))
	     result))
	))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; For output purposes we need to convert the feature back to list
;;

(defun build-list-from-sem-array (arr)
  (let* ((v nil)
	 (type (aref arr 0))
	 (vallist nil)
	 )
    ;; First we make features into a list
    (dotimes (i (- *sem-size* 1))
      (setq v (aref arr (+ i 1)))
      (cond 
       ((null v)
	(push (list (get-feature-name-by-index type (+ i 1)) '-)  vallist))
       ((eq v *default-sem-variable*)
	;; Don't print out empty features
	)
       (t (push (list (get-feature-name-by-index type (+ i 1)) v)  vallist))))
    (cons type (reverse vallist))
    ))
