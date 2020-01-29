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

(defun lexicon-define-words (&key pos templ boost-word tags words wnsense)
  (define-words-in-db (convert-to-package pos :w)  templ boost-word tags words *lexicon-data*)
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



