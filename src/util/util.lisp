(in-package "UTIL")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utility functions used by multiple modules
;;

(defun merge-features (fv1 fv2 &key (detect-conflicts nil) ;;(var-symbol '?)
				    (valtest #'(lambda (x y) (and (eql x y) x))) (keep-vars nil))
  "Merge values of 2 feature-value pairs with identical features into a single feature"
  (cond 
   ((null fv1) fv2)
   ((null fv2) fv1)
   ((eql (first fv1) (first fv2))
    (list (first fv1) (merge-values (second fv1) (second fv2) :test valtest 
				    :keep-vars keep-vars :detect-conflicts detect-conflicts
				    ;;:var-symbol var-symbol
				    ))
    )
   (t nil)
   ))

(defun merge-values (v1 v2 &key (detect-conflicts nil) (test #'(lambda (x y) (and (eql x y) x))) (keep-vars nil) (two-way nil) (new-vars nil))
  "Merge 2 values that can either be variables or symbols"
  (declare (ignore keep-vars))
  (let ((res nil))
    (cond
     ((equal v1 v2) 
      (setq res v1))
     ((is-variable-name v1)
      (setq res v2))
     ((is-variable-name v2)
      (setq res v1))
     ((and (symbolp v1) (symbolp v2))
      (setq res (if two-way
		    (or (funcall test v1 v2) (funcall test v2 v1))
		  (funcall test v1 v2))))
     ((symbolp v1) ;; v2 is a list - return v1 if it is there
      (setq res (and (member v1 (cddr v2) :test test)
		     v1)))
     ((symbolp v2) ;; v1 is a list - look for something that matches v2 there
      (setq res (merge-var-with-restr v1 (list v2) test :new-vars new-vars)))
     ;; both are lists
     ((and (is-variable-restriction v1) (is-variable-restriction v2))
      ;; both are variables
      (setq res (merge-var-with-restr v1 (cddr v2) test :new-vars new-vars))
      )
     ((is-variable-restriction v1)
      ;; only the first is a variable
      (setq res (merge-var-with-restr v1 (list v2) test :new-vars new-vars)))
     ((is-variable-restriction v2)
      ;; only the second is a variable
      (setq res (merge-var-with-restr v2 (list v1) test :new-vars new-vars)))
     )
    (when (and detect-conflicts (null res))
      (error "Incompatible values ~S and ~S" v1 v2))
    res
    ))

;; Given a variable definition, restricts the values to those in the list
;; if the restriction is empty, we return the unchanged value
;; otherwise we use a var sing and name from vardef and merge the restrictions
(defun merge-var-with-restr (vardef restr test &key (new-vars nil))
  (let ((vname (if (not new-vars) (second vardef) (gentemp (symbol-name (second vardef)))))
	(vrestr (cddr vardef))
	)
  (cond
   ((null restr) (cons (first vardef) (cons vname vrestr)))
   ((null (cddr vardef)) ;; if the variable definition is unrestricted, we add the restriction
    (cons (first vardef) (cons vname restr)))
   (t
    (let ((res (remove-if-not (lambda (x) (member x restr :test test)) vrestr)))
      (when res
	(cons (first vardef) (cons vname res)))
      ))
   )))

;; Determine if 2 values are equal given the test
(defun equal-values (v1 v2 &key (test #'eql))
  (cond 
   ;; nils are special cases and can only be equal to each other
   ((null v1) (null v2))
   ((null v2) (null v1))
   ((and (symbolp v1) (symbolp v2))
    (funcall test v1 v2))
   ((symbolp v1) ;; v2 is a list -
    (eql-symbol-variable v1 v2 :test test)
    )
   ((symbolp v2) ;; v1 is a list -
    (eql-symbol-variable v2 v1 :test test)
    )
   (t ;; both are lists
    (null (set-difference (cddr v1) (cddr v2) :test test)))
   ))

;; verify that a symbol and a variable are equal given the test   
(defun eql-symbol-variable (s v &key (test #'eql))
  (and (eql (length v) 3)
       (funcall test s (third v))))


;; Given the name and the suffix, makes a variable name out of it. Gets rid of package symbols in the process
;;
(defun right-var (name pref suf) 
  (cond
   ((and pref suf)  
    (read-from-string (format nil "~A~A~A" (symbol-name pref) (symbol-name name) (symbol-name suf))))
   (suf
    (read-from-string (format nil "~A~A" (symbol-name name) (symbol-name suf))))
   (pref
    (read-from-string (format nil "~A~A" (symbol-name pref) (symbol-name name))))
   (t name)))

(defun strip-variable-sign (n)
  "Assuming that n is a symbol and that it has a ? char in front, will strip the char"
  (when (is-variable-name n)
    (read-from-string (subseq (symbol-name n) 1))))

;; returns true if n is a varible name, i.e. starts with "?"
(defun is-variable-name (n)
  (and (symbolp n) (eql (char (symbol-name n) 0) #\?)))

(defun is-variable-symbol (n)
  "Returns t if n is a variable symbol - i.e. a package name followed by ?"
  (and (symbolp n) (equal (symbol-name n) "?"))
  )

(defun is-variable-restriction (l)
  "Returns t if l is a variable restriction - something of the form (? name v1 ... vn) where both ? and name can come from any package"
  (and (listp l)
       (symbolp (first l))
       (symbolp (second l))
       (equal (symbol-name (first l)) "?")
       ))


(defun is-positive-variable-name (n)
  "Returns t if n is a variable name from a positive restriction, i.e. does not start with !"
  (and (symbolp n) (not (equal (char (symbol-name n) 0) #\!)))
  )

;; Check if a symbol is in the form of V123 or PLOW123 (i.e., alphabets followed by numerics)
(defun is-trips-variable (var)
  (when (and (atom var)
	     (not (numberp var)))
    (let* ((string-var (string-upcase (format nil "~A" var)))
	   (temp-var (string-left-trim "ABCDEFGHIJKLMNOPQRSTUVWXYZ" string-var)))
      (when (null (equal temp-var '""))
	(if (equal (string-right-trim "0123456789" temp-var) '"")
	    t
	  nil)))))

(defun merge-in-defaults (features defaults &key (key #'first) (test #'eql))
  "Takes the list of features and merges in defaults  so that they do not override already set values
    takes and returns the list of feature-value pairs."
  (let ((result features))
    (dolist (element defaults)
      (when (not (find (funcall key element) result :key key :test test))
	(push element result)
	)
      )
    result
    ))

(defun merge-feature-lists (childlist parentlist &key (test #'eql) (two-way nil) (merge-function #'merge-features))
  (declare (ignore test two-way))
  (let ((result parentlist))
    (dolist (fval childlist)
      (setq result (merge-in-feature fval result :merge-function merge-function)))
    result))


(defun merge-in-feature (feature flist &key (key #'first) (test #'eql) (merge-function (lambda (x y) (declare (ignore x)) y)))
  (let ((olddef (find (funcall key feature) flist :key key :test test)))
    (if olddef
	(substitute (funcall merge-function olddef feature) olddef flist :test #'equal)
      ;; otherwise simplu add newdef
      (append flist (list feature))
      )
    ))


;; setting the hash entry with warning about duplicates
(defun sethash (key val table)
  (when (gethash key table)
    (util-warn "Duplicate table entry for ~S" key))
  (setf (gethash key table) val)
  )

(defun print-hash-table (table)
  "A function to print a hash table nicely"
  (maphash
   (lambda (key val) (format t "~S : ~S~%" key val))
   table
   ))

(defun get-all-keys (table &key allow-nils)
  "Returns all keys with non-nil values defined in the table"
  (let ((result nil))
    (maphash (lambda (key val)	       
	       (when (or val allow-nils) (push key result))
	       )
	     table)
    result))

(defun get-all-values (table &key allow-nils)
  "Returns all keys with non-nil values defined in the table"
  (let ((result nil))
    (maphash (lambda (key val)	       
	       (declare (ignore key))
	       (when (or val allow-nils) (push val result))
	       )
	     table)
    result))

(defun mmapcan (fn lst) "Myrosia's replacement to mapcan using
  concatenate instead of append in order to avoid circular lists"
  (unless (null lst)
    (reduce (lambda (x y) (concatenate 'list x y))
	    (mapcar fn lst)
	    ))
)

;;  This code is used to get compatability between Allegro and MCL common lisp
(defun make-system-condition (&rest x)
  "Returns the condition as appropriate for a given system"
  #-(and mcl (not openmcl))
  (apply #'make-condition x)
  #+(and mcl (not openmcl))
  (apply #'make-condition (subst :format-string :format-control x)))


;; This is used to generate all possible subsets of the list

(defun generate-subsets (list)        
  (let ((tmp nil))  
    (if list
	(progn 
	  (setf tmp (generate-subsets (rest list)))
	  (append tmp
		  (mapcar (lambda (x) (cons (first list) x)) tmp)))
      (list tmp))
    ))

 
(defun memberp (item lst)
  (if (member item lst)
      T
    nil))

;; returns the value associated with the element in an a-list
(defun assocval (el alist)
  (cdr (assoc el alist))
)

;; Converts a list of elements into a list of pairs made from
;; successive elements in the original list
(defun make-pair-list (lst)
  (cond
   ((null lst) nil)
   (t (cons (list (first lst) (second lst))
	    (make-pair-list (cddr lst))))
))

;;
;; I'm making a separate procedure that will read the file and make the list od lists
;; out of it. This is intended to facilitate transition to loadable files.
;;

(defun read-in-list-of-lists (fname)
  (let ((instream (open fname :direction :input))
	(result nil)
	(element nil))
    (setq element (read instream nil nil))
    (loop while element do  
	  (progn
	    (when (listp element) (setf result (cons element result)))
	    (setq element (read instream nil nil))))
    (close instream)
    result))

;;
;; This is a function for a newer file format, that assumes that a file is a single list
;;
(defun read-in-list-from-stream (fname &rest params &key &allow-other-keys)
  (let ((instream nil)
	(result nil))
    (setf instream (apply #'open (cons fname params)))
    (when instream
      (setq result (read instream nil nil)))
    (close instream)
    result))



(defun tree-find-if (pred tree)
  "Finds all elements in the tree forest that satisfy pred and returns them as a list"
  (cond
   ((funcall pred tree) (list tree))
   ((null tree) nil)
   ((symbolp tree) nil)
   ((not (consp tree)) nil)
   (t (mmapcan (lambda (x) (tree-find-if pred x)) tree))
   ))

(defun empty-string (str)
  (equal (string-trim '(#\Space #\Tab #\Newline) str) ""))

;; package conversion function from gf
;;   NB: add :convert-keywords option - if set to true (default to nil), it will convert
;;       keywords to the package as well (i.e., :key -> PACKAGE::KEY)
(defun convert-to-package (x &optional (package *package*) &key (convert-keywords nil) )
  "Converts non-keyword symbols (or keyword symbols as well if :convert-keywords is true)
in the given object to the given package, default *PACKAGE*."
  (cond
   ((null x)
    nil)
   ((and (keywordp x) 
	 (not convert-keywords))
    x)
   ((symbolp x)
    (intern (symbol-name x) package))
   ((consp x)
    (reuse-cons (convert-to-package (car x) package
				    :convert-keywords convert-keywords)
		(convert-to-package (cdr x) package
				    :convert-keywords convert-keywords)
		x))
   (t
    x)))

(defun reuse-cons (x y x-y)
   "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
   (if (and (eql x (car x-y)) (eql y (cdr x-y)))
       x-y
       (cons x y)))

(defun upcase-symbol-names (x)
  "Converts all symbols in the given object, including keywords, to
corresponding symbols whose name is the uppercase version of the original.
The new symbols will be interned in the same package as the original."
  (cond
   ((null x)
    nil)
   ((symbolp x)
    (intern (string-upcase (symbol-name x)) (symbol-package x)))
   ((consp x)
    (reuse-cons (upcase-symbol-names (car x))
		(upcase-symbol-names (cdr x))
		x))
   (t
    x)))

(defun append-if-necessary (x y)
  (cond
   ((null x) y)
   ((null y) x)
   (t (append x y))))

#|| lgalescu 20100513 -- this function gets redefined later in this file
    i'm only keeping it here in case someone might want to resurrect it
    with a different name -- the new definition is less efficient
(defun split-list (fn ll)
  (let ((yes nil)
        (no nil))
    (mapc #'(lambda (x) 
              (if (apply fn (list x))
                (setq yes (cons x yes))
                (setq no (cons x no))))
          ll)
    (values yes no)))
||#

;;  AKRL UTILITY FUNCTIONS

(defun gen-symbol (x)
  (gentemp (string x)))

;;  AKRL Access Routines
;;
;;   AKRL expressions are of form (AKRL-expression :content <id> :context (<reln>*))
;;     where each <reln> is as described in the AKRL manual

;; In general, there are two version of each function. One that takes the full AKRL expression
;;   and the other that works directly on an AKRL context.

(defun build-akrl (id context)
  `(AKRL-expression :content ,id :context ,context))

;; Finding the Reln for a specific ID

(defun get-def-from-akrl (id akrl)
  (get-def-from-akrl-context id (find-arg-in-act akrl :context)))

(defun get-def-from-akrl-context (id context)
  (find-if #'(lambda (x) (eq (second x) id)) context))

;; Fiding the value (typically an ID) of a role of some ID
(defun get-role-from-akrl (role id akrl)
  (if (not (and (consp akrl) (eq (car akrl) 'akrl-expression)))
      (format t "~%~%WARNING: get-role-from-akrl called with something that is not an akrl expression: ~S" akrl))
  (find-arg (get-def-from-akrl id akrl) role))

;; Find roles (i.e., keyword symbols)
(defun get-roles-from-def (def)
  (let (role-list)
    (if (listp def)
	(mapcar (lambda (x)
		  (if (keywordp x)
		      (setf role-list (append role-list (list x)))))
		  def))
    role-list))

(defun get-role-from-akrl-context (role id context)
  (find-arg (get-def-from-akrl-context id context) role))

;;  Adding or replacing a role value pair for an ID in a context (returning the new context)
(defun add-role-to-akrl-context (role id val context)
  (multiple-value-bind (objs rest)
      (split-list #'(lambda (x) (eq (second x) id)) context)
    (let ((newobj (append (remove-arg (car objs) role) (list role val))))
      (cons newobj rest))))

(defun remove-def-from-akrl (id akrl)
  (build-akrl id (remove-def-from-akrl-context id (find-arg-in-act akrl :context))))

(defun remove-def-from-akrl-context (id context)
  (remove-if #'(lambda (x) (eq (second x) id)) context))

;; Replacing the entire definition of an ID in a context (returning the new context)
(defun replace-def-in-akrl-context (def context)
  "Replaces the definition of a term in the context with a new definition DEF"
  (let* ((id (second def))
	 (reduced-context (remove-if #'(lambda (x) (eq (second x) id)) context)))
    (cons def reduced-context)))

;;  Adding or replacing a role value in a RELN (returning the new relation)
(defun replace-Reln-role (rolename act val)
  (if (member rolename act)
      (replace-if-this-one rolename act  val)
    (append act (list rolename val))))

;;  Accessing the role of a RELN
(defun get-Reln-role (rolename reln)
  (find-arg reln rolename))
  

(defun replace-if-this-one (rolename args val)
  (when args
    (if (eq rolename (car args))
	(cons rolename (cons val (cddr args)))
      (cons (car args) (cons (cadr args) (replace-if-this-one rolename (cddr args) val))))
    ))


(defun remove-unused-terms (prop terms)
  "This removes terms not used in the prop -- this is not called much, so it is
inefficiently implmented using remove-unusued-context"
  (let* ((p (find-arg-in-act  prop :lf))
	(context (mapcar #'(lambda (x) (find-arg-in-act x :lf)) terms))
	(used (mapcar #'cadr (remove-unused-context p context))))
    (remove-if #'(lambda (x) (not (member (find-arg-in-act x :var) used))) terms)))

(defun remove-unused-context-with-root (root context)
  (remove-unused-context (get-def-from-akrl-context root context) context))

(defun remove-unused-context (prop context)
  "This removes AKRL definitions of objects not 'reachable' from the PROP.
   We gather all the defns of objects used in the PROP, and then in defns of objects used there, etc"
  (let ((ids (get-ids prop))
	(done nil)) 
    (multiple-value-bind
	(found out)
	(split-list  #'(lambda (x) (member (cadr x) ids))  context)
      (setq done found)
      (loop
       while (and found out)
       DO (let ((next (car found)))
	    (multiple-value-bind
	      (in1 out1)
	      (Remove-unused-context next out)
	      (setq done (append done in1))
	      (setq found (cdr found))
	      (setq out out1)
	      )))
      (values done out))))


(defun get-ids (x)
  (cond ((null x) nil)
	((atom x) (list x))
	((consp x)
	 (append (get-ids (car x)) (get-ids (cdr x))))))

;;; EASY AKRL term construction

(defun generate-akrl-relns-from-specs (relns)
  "takes a specification of relns in form (type Role1 val1 ... rolen valn)
     where a value may be a nested reln specification"
  (when relns
    (multiple-value-bind (firstid firstterm)
	(generate-akrl-reln (car relns))
      (multiple-value-bind (ids terms)
	  (generate-akrl-relns-from-specs (cdr relns))
	(values (cons firstid ids)
		(append firstterm terms))
	)))
  )

(defvar *additional-terms* nil)

(defun generate-akrl-reln (reln)
  "generates AKRL terms for a reln specification:
     e.g., (ONT::|Example| :|object| (ONT::|Conditional| :role john)))"
  (let* ((id (gen-symbol 'i))
	 (*additional-terms* nil))
    (values id (cons (generate-akrl-reln-with-id reln id)
		     (mapcar #'(lambda (x) (generate-akrl-reln-with-id (cdr x) (car x))) *additional-terms*))
	    )
    ))

(defun generate-akrl-reln-with-id (reln id)
  (generate-akrl-args (list (car reln) id :instance-of (cadr reln)) (cddr reln)))
	

(defun generate-akrl-args (reln args)
  "replaces nested terms with an id, and stored info in *additional-terms* list"
  (if args
      (let ((role (car args))
	    (val (cadr args)))
	(cond ((symbolp val)
	       (generate-akrl-args (append reln (list role val)) (cddr args)))
	      ;; otherwise we have a nested specification
	      ((consp val)
	       (let ((id (gen-symbol 'i)))
		 (setq *additional-terms* (cons (cons id val) *additional-terms*))
		 (generate-akrl-args (append reln (list role id)) (cddr args)))
	      )))
      reln))

;;  AKRL pattern matching
;; this supports pattern matching into AKRL expressions. The patterns provide a convenient way
;;   to follow chains of roles in a single expression.
;;  e.g., the pattern syntax is
;;    <pattern> ::= (akrl-pattern <type> (<role-pattern>)*)
;;    <role-pattern> ::= (:spec <val>) | (:var <val>) | (<role>* <val>)
;;   <val> ::= <atom> | <var>
;;   <var> ::= ?<atom>
;;  The pattern language supports the "psuedo-roles :spec :var and :type for matching the first, second and third elements of an AKRL element respectively
;;   e.g., Given
;;          (AKRL-EXPRESSION :content t1
;;                  :context ((RELN t1 :instance-of ONT::TEACH :agent USER :subject V1)
;;                            (RELN v1 :instance-of ONT::BUY :theme v2)
;;                            (A v2 :instance-of ONT::COMPUTER)))
;;  the pattern (AKRL-PATTERN ONT::TEACH (:subject ?proc))  would match and bind ?proc to V1
;;  the pattern (AKRL-PATTERN ONT::TEACH (:subject :instance-of ?x))  would match and bind ?x to BUY
;;  the pattern (AKRL-PATTERN ONT::TEACH (:subject (AKRL-PATTERN ONT::BUY (:THEME ?CC)))) would match and bind ?CC to V2

(defparameter *success* '((nil nil)))

;;  AKRL-MATCH takes an AKRL-PATTERN, and ARKL-EXPRESSION, and an answer key
;;  e.g., (AKRL-MATCH '(AKRL-PATTERN ONT::TEACH (:subject :theme ?proc) (:agent ?ag))
;;                    '(AKRL-EXPRESSION
;;                      :content t1
;;                      :context
;;                        ((RELN t1 :instance-of ONT::TEACH :agent USER :subject V1)
;;                         (RELN v1 :instance-of ONT::BUY :theme v2) (A v2 :instance-of ONT::COMPUTER))
;;                    '(?proc ?ag))
;;   returns (V2 USER)

(defun AKRL-MATCH (akrl-pattern akrl-expr answer-format)
  "matches a pattern against an AKRL expression and returns a binding list of form ((<var-name> <value>)*)"
  (akrl-match-into-context (find-arg-in-act akrl-expr :content)
			   akrl-pattern
			   (find-arg-in-act akrl-expr :context)
			   answer-format))

;;  A version of AKRL-MATCH where you provide the :content ID with the context.
(defun akrl-match-into-context (id akrl-pattern context answer-format)
  (let ((bndgs (akrl-match-to-def (get-def-from-akrl-context id context) (cdr akrl-pattern) context)))
    (if bndgs
	(if answer-format (substitute-bndgs answer-format bndgs)
	  bndgs))))

(defun akrl-match-to-def (def pattern context)
  (when pattern
    (let* ((type (car pattern))
	   (bndgs (match-pat-to-object type (find-arg def :instance-of) context)))
      (if bndgs
	  (merge-bindings bndgs (match-roles def (substitute-bndgs (cdr pattern) bndgs) context))
	))))

(defun match-roles (def role-pats context)
  "This matches all the role values in the pattern, which may be expressed in simple keyword form,
      of using nested keyword patterns"
  (if role-pats
      (let* ((rp (car role-pats))
	     (r (if (consp rp) (car rp) rp))
	     (v (get-akrl-role-from-def def r)))
	(if v
	    (let ((bndgs (match-rolepat-to-object (if (consp rp) (cdr rp) (cadr role-pats)) v context)))
	      (when bndgs
		(merge-bindings bndgs (match-roles def (if (consp rp) (cdr role-pats)
							 (cddr role-pats))
						   context))
		))
	))
    *success*))

(defun get-akrl-role-from-def (def r)
  "like find-arg but supports the special roles :spec :var and :type in patterns"
  (case r
    (:spec (car def))
    (:var (second def))
    (otherwise (find-arg def r))))
   

(defun match-rolepat-to-object (rp v context)
  "handles chains of roles if necessary"
  (if (null (cdr rp))
      ;; we have a value
      (match-pat-to-object (car rp) v context)
    ;; else follow the chain
    (let ((newdef (get-def-from-akrl-context v context)))
      (if newdef (match-rolepat-to-object (cdr rp) (get-akrl-role-from-def newdef (car rp)) context)))
    ))

(defun match-pat-to-object (pat val context)
  "We have a pattern and value"
  (cond
   ((variable-p pat)
    (list (list pat val)))
   ((and (consp pat) (eq (car pat) 'AKRL-PATTERN) (symbolp val))
    (akrl-match-into-context val pat context nil))
   (t
    (if (eq pat (simplify-val val context))
	*success*)))
  )
 
(defun simplify-val (val context)
  "handles AKRL :EQUALS case"
  (let ((def (or (get-def-from-akrl-context val context) val)))
    (if (symbolp def)
	def
      (if (consp def) 
	  (or (find-arg def :equals)
	    def)))))

(defun variable-p (x)
  (and (symbolp x)
       (eql (char (symbol-name x) 0) #\?)
       ))

(defun merge-bindings (bndgs1 bndgs2)
  (when (and bndgs1 bndgs2)
    (cond
     ((eq bndgs1 *success*) bndgs2)
     ((eq bndgs2 *success*) bndgs1)
     (t (append bndgs1 bndgs2)))))

(defun substitute-bndgs (expr bndgs)
  (if (or (null bndgs) (eq bndgs *success*))
      expr
    (subst-each expr bndgs)))

(defun subst-each (expr bndgs)
  (if (null bndgs)
      expr
    (subst-each (subst (cadar bndgs) (caar bndgs) expr)
		(cdr bndgs))))
   

(defun split-list (fn ll)
  "This divides a list into two lists depending on whether FN is true of the element - it retains the original ordering of elements"
  (let ((yes nil)
        (no nil))
    (mapc #'(lambda (x) 
              (if (apply fn (list x))
                (setq yes (cons x yes))
                (setq no (cons x no))))
          ll)
    (values (reverse yes) (reverse no))))
