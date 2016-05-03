;;
;; structures.lisp
;;

(in-package "PARSER")

;; From sem-features.lisp
(declaim (special *sem-size* *default-sem-variable*))


;; JOEL 9/29/02
(defvar +unknown-words+ NIL)

;;  structure for position indexed objects

(defstruct indexedObject
  object start end)

;;; From Chart.lisp

(defstruct entry
  constit start end rhs name rule-id prob prob-aux first-cat 
  size  ;; size is that actual number of non-space characters that the entry covers
  skeleton  ;; the semantic skeleton for
  )

(defstruct arc
  mother pre post start end rule-id prob local-vars foot-feats prob-aux first-cat)

(defstruct agenda-item
  type prob start end entry id arc bndgs score)

;;; From GrammarandLexicon.lisp
;;   full CONSTIT entries for word retrieval

(defstruct lex-entry constit id prob nonhierarchy-lf)

;;  Structures for hierarchical lexicon

(defstruct lex-node id feats parent)

(defstruct (rule
            (:print-function
	     (lambda (p s k)
	       (declare (ignore k))
	       (Format s "~%<~S~%   ~S ~S :prob=~S>"
		       (rule-lhs p) (rule-id p) (rule-rhs p)
		       (rule-prob p))
	       (if (rule-*-flag p) (Format s "*-flag-enabled")))))
  lhs id rhs prob var-list *-flag)

;;; From FeatureHandling.lisp

(defstruct (constit (:print-function
		     (lambda (p s k)
		       (declare (ignore k))
		       (format-constit p nil s))))
  cat feats head optional
  )

(defstruct (var
            (:print-function
             (lambda (p s k)
               (declare (ignore k))
	       (format-var p nil s))))
  name values non-empty)

;;  Compiled Grammar and Lexicon formats

;;  Note: the ACTIVE-RULES slot is used for parsing, and always reflects
;;   a merging of the RULES and LEX-RULES slots. These are kept separately
;;   so that changing the lexicon used with the grammar is easy to handle

(defvar *default-lexical-cats*  '(w::n w::v w::adj w::art w::p w::aux w::pro w::qdet w::pp-wrd w::name w::to))

(defstruct cgrammar
  rules					; grammar RULES in internal format
  ids					; list of rule ids
  (index (make-hash-table))		; hashtable index of rules and lex-rules by first constituent cat on RHS
  (default-rule-prob 0.98)		; default rule probability
  (lex (make-hash-table :test #'equal :size 1)) ; compiled lexical entries
  lex-rules				; lexical rules for compound lexical items
  gapsEnabled				; is GAP processing enabled with this grammar
  gap-cat-feat-lists			; GAP declarations if enabled
  semenabled				;declarations for SEM processing with this grammar
  (lexicalCats *default-lexical-cats*)
  )

;; The Default grammar is initally empty

(defvar *default-grammar* (make-cgrammar))

;;  CONSTANTS FOR CONTROLLING PARSER BEHAVIOR
;;  This constant indicates a successful unification

(defparameter *success* '((NIL NIL)))

;;  Handling unknown words

(defvar *ignore-unknown-words* t)
(defvar *unknown-word-penalty* .75)
(defvar *skip-constituent-penalty* 
  `(.97 
    (w::<SIL> 1)
    (w::punc-period .9)
    ))

(defvar *trace-calls* nil)

(defun set-unknown-word-penalty (&optional penalty)
  (cond
   ((null penalty)
    (setq *ignore-unknown-words* t)
    (setq *unknown-word-penalty* .75))
   ((numberp penalty)
    (cond ((= penalty 0)
	   (setq *ignore-unknown-words* nil))
	  ((and (>= penalty 0) (<= penalty 1))
	   (setq *ignore-unknown-words* t)
	   (setq *unknown-word-penalty* penalty))
	  (t
	   (parser-warn "unknown word penalty must be between 0 and 1.~% You specified ~S" penalty))))
   (t
    (parser-warn "unknown word penalty must be between 0 and 1.~%  You specified ~S" penalty))))

(defun unknown-word-penalty nil
  *unknown-word-penalty*)

(defun set-skip-constituent-penalty (lex  penalty)
  (cond
    ((and (numberp penalty) (>= penalty 0) (<= penalty 1))
     (setq *skip-constituent-penalty* (cons (car *skip-constituent-penalty*)
					    (cons (list lex penalty)
						  (cdr *skip-constituent-penalty*)))))
   (t
    (parser-warn "Skip constituent penalty must be between 0 and 1.~%  You specified ~S" penalty))))

(defun skip-constituent-penalty nil
  *skip-constituent-penalty*)

;;(eval-when (load)
  (defvar *empty-constit*
      (make-constit :cat '- :feats '((w::var -))))

;;  CONSTIT to LIST conversion

(defun constit-to-list (c)
  (cond ((null c) nil)
	((and (arrayp c) (not (stringp c)));; got a sem array
	 (array-to-list c))
	((constit-p c)
	 (list '%
               (constit-to-list (constit-cat c))
               (constit-to-list
                (remove-if #'(lambda (fv)
                               (null (cadr fv)))
                           (constit-feats c)))))
	((var-p c)
	 (list '? (convert-to-package (var-name c)) (constit-to-list (var-values c))))
	((consp c)
         ;; check for conjunctions first
         (if (eq (car c) '&)
           (cons '&
                 (constit-to-list (flatten-ands (cdr c))))
	   ;; otherwise a standard list structure
           (cons (constit-to-list (car c))
	         (constit-to-list (cdr c)))))
	(t c)))

(defun flatten-ands (ll)
  "removes nested ANDs and eliminates - or NIL"
  (if (consp ll)
    (remove-if #'(lambda (y)
                   (or (null y) (eq y '-) (and (consp y) (eq (car y) '-))))
             (mapcan #'(lambda (x)
	                 (if (and (listp x) (eq (car x) '&))
                           (flatten-ands (cdr x))
		           (list x)))
	             ll))
    ll))

(defun list-to-constit (l)
  (init-var-table)
  (list-to-constit1 l))

(defun list-to-constit1 (l)
  (if (consp l)
      (case (car l)
	(% (make-constit :cat (second l) :feats (list-to-constit1 (third l))))
	(? (or (get-var (second l)) (add-var (second l)
					     (make-var :name (second l)
						       :values (list-to-constit1 (third l))))))
	(otherwise
	 (cons (list-to-constit1 (car l))
	       (list-to-constit1 (cdr l)))))
    l))

(defun array-to-list (arr)
  (let* ((v nil)
	 (type (aref arr 0))
	 (values nil) (fval nil)
	 )
    (dotimes (i (- *sem-size* 1))
      (setq v (aref arr (+ 1 i)))
      (cond
       ((null v) 
	(setq fval (list 
		    (get-feature-name-by-index type (+ 1 i)) '-))       
	)
       ((eq v *default-sem-variable*)
	;; Don't print out empty features
	 ;; (format stream "(~S " (get-feature-name-by-index type i))
	;;(format stream "?)")
	(setq fval nil)
	)
       (t
	(setq fval (list (get-feature-name-by-index type (+ 1 i))
			 (constit-to-list v)))
	)
       )
      (when (and fval (car fval))
	(push fval values))
      )
      (list* '$  (constit-to-list type) values)
 	  
    ))

;;  SEMANTIC FEATURES

(defvar *use-complex-sem-features* t)

(defun convert-to-syntax-package (x)
  (util::convert-to-package x :w)
  )

;;(defvar *sem-types* '(sem phys-obj situation proposition abstr-obj))
;;(defvar *sem-features* '((sem type)
;;			 (phys-obj form origin functn mobility information intentional container
;;				   spatial-abstraction type)
;;			 (situation aspect cause locative trajectory situationTheme
;;				    time information type container intentional)
;;			 (proposition information intentional container)
;;			 (abstr-obj functn measure scale information intentional container type temporal-abstraction)
;;			 ))


