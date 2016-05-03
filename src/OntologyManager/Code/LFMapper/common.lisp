;; Nate Chambers
;;
;; Simple formatting and validation functions that are/may be used by
;; multiple files.
;;

(in-package "ONTOLOGYMANAGER")

(defun subst-bindings (bindings form)
  (simplekb::subst-bindings bindings form))

(defun is-transform-var (var)
  "Return true if it is a valid variable for an LF Transform, nil otherwise.
   e.g. ?vv"
  (and (symbolp var)
       (eql (char (symbol-name var) 0) '#\?)))

(defun extract-vars (form)
  "@returns A list of all the variables in the given form"
  (cond ((null form) nil)
	((listp form) (append (extract-vars (car form))
			      (extract-vars (cdr form))))
	((is-transform-var form) (list form))
	(t nil)))

(defun signal-error (str &rest rest)
  "Just standard output for now..."
  (apply #'format (append (list 't str) rest)))

(defun assoc-all (var bindings)
  "@returns A list of values for every assoc-list of var"
  (cond ((or (null bindings) (not (listp (first bindings)))) nil)
	(t (if (eq var (first (first bindings)))
	       (cons (second (first bindings)) (assoc-all var (rest bindings)))
	     (assoc-all var (rest bindings))))))

(defun get-package (x)
  "@returns The package of x"
  (let* ((str (format nil "~S" x))
	 (colon (position #\: str)))
    (if colon (subseq str 0 colon) "ONTOLOGYMANAGER")))

(defun cons-unique (a ll)
  (if (member a ll) ll (cons a ll)))

(defun make-pairs (x)
  "@param x A list with even number of elements"
  (if (null x) nil
    (cons (list (first x) (second x)) (make-pairs (cddr x)))))

(defun unique-id ()
  "Called when creating new KR frames.  e.g. returns N47"
  (util::convert-to-package (gentemp "N") :ont))

(defun remove-keyword-arg (hal key)
  "returns hal in-tact, but removes the first occurence of key and its value
  i.e. (remove-keyword-arg '(:him us :her you hi) :him) -> (:HER YOU HI)"
  (cond ((null hal) nil)
	((atom hal) (list hal))
	((eq key (first hal)) (cddr hal))
	(t (cons (first hal) (remove-keyword-arg (rest hal) key)))))

(defun remove-pred (type form)
  "@desc Removes a predicate from a lambda form
     (lambda v1 (type v1 hello) (agent v1 hi)) -> (lambda v1 (type v1 hello))"
  (cond ((valid-kr-object form)
	 (let ((preds (rest (rest form)))
	       (newpreds nil))
	   (dolist (pred preds)
	     (if (eq (first pred) type) nil
	       (setf newpreds (cons pred newpreds))))
	   (append (list (first form) (second form))
		   (reverse newpreds))
	   ))
	(t form)))

;; This is a separate function because LFMapper was created external of
;; the OM.  I am leaving it here in case it may move in the future.  This
;; provides a nice black box for LF access.
(defun lf-subtype (type1 type2)
  "@returns True if type1 is a descendent of type2"
  (subtype type1 type2))

(defun remove-nils (ll)
  (if (null ll) nil
    (if (first ll) (cons (first ll) (remove-nils (rest ll)))
      (remove-nils (rest ll)))))

(defun split-hyphens (word)
  "return a list of words that were separated by hyphens (but not LF types)
GO-HOME-YOU -> (GO HOME YOU)"
  (cond ((not (stringp word)) nil)
	(t (mapcar #'read-from-string
		   (util::split-string (string word) :on #\-))
	   )))

;; this is defined better in src/util/string.lisp
;(defun split-string (str &key (on #\Space))
;  "@desc Returns a list of strings which are the sequences in 'str' that
;         are separated by the character 'on'"
;  (dotimes (x (length str) (list str))
;    (if (equal on (char str x))
;	(return-from split-string
;	  (cons (subseq str 0 x)
;		(split-string (subseq str (1+ x) (length str)) :on on))))
;    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRIPS LF functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun utt-to-terms (utt)
  "@param utt A TRIPS utt list.
   @returns The list of terms after the :terms role in the main utt, along
            with the SPEECHACT term removed."
  (let ((terms (get-keyword-arg utt :terms)))
    (cond (terms
	   ;; extract just the LF
	   (setf terms (mapcar (lambda (x) (get-keyword-arg x :lf))
			       terms))
	   ;; remove the SPEECHACT term
	   (remove '(ont::speechact speechact) terms
		   :test (lambda (x y) (member (first y) x))))
	  (t utt))))

(defun remove-setofs (terms)
  (let (bindings newterms)
    (dolist (term terms)
      (multiple-value-bind (newterm binds)
	  (remove-setof term)
	(setf bindings (append binds bindings))
	(setf newterms (cons newterm newterms)))
      )
    (subst-lf-bindings bindings (reverse newterms))))

(defun remove-setof (term)
  (let ((var (second term))
	(type (third term))
	bindings)
    (cond ((and (listp type) (member (first type) '(ont::set-of set-of)))
	   ;; remove set-of wrapper
	   (setf type (second type)) 
	   ;; ignore Lambda list  i.e. (L v12 (:* ..))
;	   (cond ((and (listp type) (eq 'L (first type)))
	   (cond ((and (listp type)
		       (member (first type) '(ont::lambda ont::L L lambda))
		       (not (eq (second type) var)))
		  (setf bindings (list (cons (second type) var)))
;		  (setf var (second type))
		  (setf type (rest (rest type))))
		 (t (setf type (list type))))
	   ;; make the new term
	   (values (append (list (first term) var) type (cdddr term))
		   bindings))
	  (t (values term nil)))))

(defun extract-lftype (LF)
  "@param LF e.g. (THE V23847 (:* ONT::type ...
   @returns The ONT::type given an LF in any form"
  (let ((mid (third LF)))
    (if (and (listp mid) (equal 'SET-OF (first mid))) 
	(setf mid (cadr mid)))
    (if (and (listp mid) (equal 'L (first mid)))
	(setf mid (extract-lftype mid)))
    (cond ((and (listp mid) (eq '<OR> (first mid)))
	   (second mid))
	  ((listp mid) (second mid))
	  (t mid))))

(defun extract-lflex (LF)
  "@param LF e.g. (THE V23847 (:* ONT::type ...
   @returns The lexical item in the LF"
  (let ((mid (third LF)))
    (if (and (listp mid) (equal 'SET-OF (first mid)))
	(setf mid (cadr mid)))
    (if (and (listp mid) (equal 'L (first mid)))
	(setf mid (extract-lftype mid)))
    (cond ((and (listp mid) (eq '<OR> (first mid)))
	   nil)
	  ((listp mid) (third mid))
	  (t nil))))

(defun term-with-var (terms var)
  "Return the term with variable var from the list of LF terms."
  (dolist (term terms)
    (if (eql var (get-termvar term))
	(return-from term-with-var term)))
  nil)

(defun get-termvar (term)
  "Return the variable of the LF term.  e.g. (F V12 ONT::MOVE ...) -> V12"
  (if (and (listp term) (< 2 (length term)))
      (second term) nil))

(defun get-all-role-values (term role)
  "Return a list of values for the given role.  Collapses list values into one.
   i.e. for assoc-with:
   (F V1 ONT::COMPUTER :assoc-with v2 :assoc-with v3) -> (v2 v3)
   (F V1 ONT::COMPUTER :assoc-with v2) -> (v2)
   (F V1 ONT::COMPUTER :mods (v2 v3)) -> (v2 v3)
   (F V1 ONT::COMPUTER :mods (v2 v3) :mods v1) -> (v2 v3 v1)"
  (cond ((or (null term) (atom term)) nil)
	((eq role (first term))
	 (if (and (listp (second term)) (is-lf-variable (first (second term))))
	     ;; collapse list values
	     (append (second term) (get-all-role-values (cddr term) role))
	   (cons (second term) (get-all-role-values (cddr term) role))))
	(t (get-all-role-values (rest term) role))))

(defun subsume-role-values (primary secondary)
  "@param primary A list of role/value pairs (:theme v1 :agent v2 ...)
   @returns The merging of the two role lists, and choosing the primary value if
            the role appears in both"
  (let (primes final)
    (dolist (pair (make-pairs secondary))
      (setq primes (get-all-role-values primary (car pair)))
      ;; allow duplicate roles of certain role types
      (cond ((and (member (car pair) '(:mods :assoc-with))
		  (not (member (second pair) primes)))
	     (setq final (append final pair)))
	    ;; if the role doesn't appear in the primary roles, add it
	    ((not primes)
	     (setq final (append final pair))))
      )
    (append primary final)))

(defun is-lf-feature (feat)
  "returns true if feat is a feature  e.g. :THEME"
  (keywordp feat))

(defun is-lf-variable (var)
  (and (symbolp var)
       (> (length (symbol-name var)) 1)
       (not (digit-char-p (char (symbol-name var) 0)))
       (digit-char-p (char (symbol-name var) 1))))

(defun subst-lf-bindings (bindings x)
  "Substitute the value of variables in bindings into x,
  taking recursively bound variables into account."
  (cond ((null bindings) x)
        ((and (is-lf-variable x) (assoc x bindings))
         (subst-lf-bindings bindings (simplekb::lookup x bindings)))
        ((atom x) x)
        (t (reuse-cons (subst-lf-bindings bindings (car x))
                       (subst-lf-bindings bindings (cdr x))
                       x))))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))
