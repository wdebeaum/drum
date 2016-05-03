;;;;
;;;; make-new-lex.lisp
;;;;
;;;;
 
(in-package "LEXICONMANAGER")


(defun init-lexicon-db ()
  "Creates a new empty lexicon structure, putting in pointers for LF and KR ontologies"
  (make-lexicon-db
   :word-table (make-hash-table)
   :synt-table (make-hash-table)
   :lf-table (make-hash-table)
   ;; 2004 -- now we query for these from the OM
   ;;   :lf-ontology *lf-ontology*
   ;;   :kr-ontology *kr-ontology*
  ))
		  

;; Managing new data structures
(defvar *lexicon-data* (init-lexicon-db))

;; 
;; Macros for words and templates
;;

(defmacro define-words (&rest args)
  `(apply #'lexicon-define-words ',args))

(defmacro define-list-of-words (&rest args)
  `(apply #'lexicon-define-list-of-words ',args))

(defun lexicon-define-words (&key pos templ boost-word tags words)
  (define-words-in-db pos templ boost-word tags words *lexicon-data*)
  )

(defun lexicon-define-list-of-words (&key boost-word pos senses words tags)
  (mapcar #'(LAMBDA (w)
		    (lexicon-define-words :pos pos :boost-word boost-word :tags tags :words `((,w (senses ,@senses)))))
	  words))

(defmacro define-templates (&rest args)
  `(apply #'lexicon-define-templates ',args))

(defun lexicon-define-templates (args)  
  (mapcar #'(lambda (templ)
	      (insert-template templ (lexicon-db-synt-table *lexicon-data*))
	      )
	  args))

;;; swift commenting out define-adjective-system -- no longer used
;(defmacro define-adjective-system (&rest args)
;  `(define-adjective-sys ',args))
;
;(defun define-adjective-sys (args)
;  (Let* 
;    (;;(nominal  (find-arg args :nom))
;     (lf-parent (find-arg args :scale))
;     (type (find-arg args :type))
;     (argsems (find-arg args :domains))
;     (values (find-arg args :values))
;     ;;(units (find-arg args :units))
;     )
;     ;;  First define the LF type
;    (eval (list 'define-type
;                lf-parent
;                :parent (case type
;                          (ordered-scale 'LF_non-measure-ordered-domain)
;                          )
;                :arguments
;                `((:essential argument ,(car argsems)))
;                ))
;             
;    (mapcar #'(lambda (argsem)
;                (declare (ignore argsem))
;                (mapcar #'(lambda (a)
;                            (define-adjectives a lf-parent))
;                        values))
;            argsems))
;  )
;
;(defun define-adjectives (adjs parent)
;  "Takes a list, the first value being morph features and the rest being adjectives with those
; features"
;  (if (and (consp (car adjs)) (find-arg (car adjs) :forms))
;    (let* ((morph (car adjs))
;           )
;      (mapcar #'(lambda (a)
;                  (add-adj a morph parent))
;              (cdr adjs)))
;    (mapcar #'(lambda (a)
;                (add-adj a nil parent))
;            adjs)))
;
;(defun add-adj (adj morph parent)
;  "adds a adjective to the lexicon DB, using forms and morph features.
;   If adj is a list, it is defining idiosyncratuc morph features"
;  (let ((word (if (consp adj) (car adj) adj))
;        (newmorph (if (consp adj) (append (cdr adj) morph) morph)))
;    (add-entries-for-word 
;     (make-vocabulary-entry 
;      :name (gen-id adj)
;      :pos 'ADJ
;      :word adj
;      :wfeats `((w::MORPH ,newmorph))
;      :senses (list 
;               (make-sense-definition :pos 'ADJ :lf-parent parent :lf-form word
;                                      :templ 'simple-adj-templ :params nil
;                             :pref 1))
;      )
;     *lexicon-data*)))


