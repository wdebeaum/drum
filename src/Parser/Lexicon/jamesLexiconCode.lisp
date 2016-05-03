;; Lexicon access functions

(in-package "PARSER")

(defvar *lexicon-data* nil)

(defun build-new-lexicon nil
  "Takes Myrosia's *word-tables* and builds a hash table by word"
  (setq *lexicon-data* (make-hash-table))
  (mapcar #'(lambda (x)
              (maphash #'(Lambda (word entry)
                           (add-entries-for-word entry))
                       (cdr x)))
          *word-tables*))

(defun add-entries-for-word (entry)
  "This adds the word and all its morphological variants to the *lexicon-data* table"
  (if (vocabulary-entry-p entry)
    (let* ((morph (assoc 'MORPH (vocabulary-entry-wfeats entry)))
           ;;  first check vocab entry, then template, then use default
           (morph-features (or (cadr morph)
                              ;; (find-morph-features-in-template (vocabulary-entry-templ entry))
                               (get-morph-default (vocabulary-entry-pos entry))))
           (variants (generate-morph-variants (vocabulary-entry-word entry) morph-features entry)))
        (if variants
          (mapcar #'(lambda (x)
                      (push  (second x)
                             (gethash (first x) *lexicon-data*)))
                  variants)
          ;; must have had (MORPH nil), so add it as it is
          (push (list :none entry)
           (gethash (vocabulary-entry-word entry) *lexicon-data*)))
        )))

(defun get-morph-default (pos)
  (case pos
    (v '((morph (:forms (-vb)))))
    (n '((morph (:forms (-s-3p)))))
    (adv '((morph (:forms (-none)))))
    (adj '((morph (:forms (-none)))))
    (otherwise nil)))

(defun find-morph-features-in-template (name)
  "Looks at the template namedd in thge arg and returns the value of the MORPH feature
      if it exists"
  (let ((templ (gethash name *synt-table*)))
    (if (syntax-template-p templ)
      (cdr (assoc 'MORPH (syntax-template-syntax templ))))))

;;=======================
;;    Code to generate morphological variants of a base form in the lexicon table
;;    To save time and space, we don't generate the actual modified entry until needed by the parser.
;;
;;   Entries in the table are of the form (<morphfeat> <base entry>)
;;   This information is later used to generate the real lexicon entry for the word when needed by the parser

(defun generate-morph-variants (word features entry)
  "This generates the morphological variants of the given word and sets up
     values for the lexicon table that are of the form (<form> <base entry>)
     No change is made to the entry until the wors is actually needed"
    
  (let ((forms (find-arg features :forms)))
    (if (null forms)
      (list (list word (list :none entry)))
      (mapcan #'(lambda (form)
                  (case form
                    ;;  normal verb forms
                    (-vb (mapcar #'(lambda (f)
                                     (gen-variant word f features entry))
                                 '(:12s123pbase :3s :ing :past :pastpart)))
                    ;; normal noun forms
                    (-s-3p
                     (mapcar #'(lambda (f)
                                 (gen-variant word f features entry))
                             '(:sing :plur)))
                    (-load
                     (mapcar #'(lambda (f)
                                 (gen-variant word f features entry))
                             '(:load :load-p)))
                    ;; adjective forms
                    (-er
                     (mapcar #'(lambda (f)
                                 (gen-variant word f features entry))
                             '(:none :er :est)))
                    (-erdouble
                     (mapcar #'(lambda (f)
                                 (gen-variant (double-last-consonant word) f features entry))
                             '(:er :est)))
                    ;; words that have no forms
                    (-none
                     (list (list word (list :none entry))))
                    (otherwise
                     (parser-warn "Unknown Morph FORM ~S in defn ~S" form entry))))
              forms))))

(defun gen-variant (base form features entry)
  "takes one form (e.g., :3s :ing) and generates the entry"
  (let ((irreg (get-morph-feat features form)))
    ;; pastparticiple defaults to the irregular past form if it is not specified
    (if (and (eq form :pastpart) (null irreg))
      (setq irreg (get-morph-feat features :past)))
      (list (or irreg 
                (gen-standard-morph-variant form base))
                (list form entry))))

(defun gen-standard-morph-variant (form base)
  "Generates a standard morphological variant"
  (case form
    (:none base)
    ;; verb forms
    (:12s123pbase base)
    (:3s (add-suffix base "S"))
    (:ing (add-suffix base "ING"))
    (:past (add-suffix base "ED"))
    (:pastpart (add-suffix base "ED"))
    ;; noun forms
    (:sing base)
    (:plur (add-suffix base "S"))
    (:load (add-suffix base "LOAD"))
    (:load-p (add-suffix base "LOADS"))
    ;; adjective forms
    (:er (add-suffix base "ER"))
    (:est (add-suffix base "EST"))
    ))
  
;;========================================================
;;   Code to generate lexicon entries for the parser from the lexicon table
;;

(defun get-word-constit (w)
  (let ((entries (gethash w *lexicon-data*)))
    (mapcan #'(lambda (e)
                (gen-lexicon-constits w e))
            entries)))

(defun gen-lexicon-constits (word lex-entry)
  (let* ((morphfeat (first lex-entry))
         (entry (modify-entry word morphfeat (second lex-entry)))
         (def (make-word-sense-definitions entry *synt-table* *lf-ontology*))
         (kr-senses (add-kr-info def *lf-ontology* *kr-ontology*))
         )
  kr-senses
  ))

(defun modify-entry (word morphfeat entry)
  "Returns a new entry with the appropriate features for this morphological feature"
  (if (vocabulary-entry-p entry)
    (let ((new-entry (copy-vocabulary-entry entry)))
      (setf (vocabulary-entry-word new-entry) word)  ;; changes root form to desired form
      (if (member morphfeat '(:load :loads))
        (do-complex-modify word morphfeat new-entry)
        ;; or do a simple modify which is just changes to the SYNTAX feature
        (progn
          (setf (vocabulary-entry-wfeats new-entry)
              (generate-morph-variant-features word morphfeat (vocabulary-entry-wfeats new-entry)))
          new-entry)))
    (parser-warn "Illegal entry found for word ~S: ~S" word entry)))

(defun do-complex-modify (word morphfeat new-entry)
  "This generate new entries in which any aspect of the change may be changed"
  (:load
   (do-load-modify morphfeat new-entry '3s))
  
  (:load-p 
   (do-load-modify morphfeat new-entry '3p))
  )
     
  
(defun do-load-modify (morphfeat new-entry agr)
  (setf (word-sense-definition-syntax new-entry)
        (replace-feat 
         (replace-feat 
          (replace-feat (word-sense-definition-syntax new-entry) 'sem '($ abstr-obj))
          'sort 'unit)
         'agr agr))
  (setf (word-sense-definition-templ) nil) ;; working here - what do we update?
  )

(defun generate-morph-variant-features (word morphfeat syntaxfeatures)
  "generates the appropriate syntactic features for the morphological variant indicated
     by the morphfeat. The feature uniquely identifies the part of speech and the part
     of speech doesn't change. i.e., we can't use this function for generate an adverb from
     an adjective"
 (let ((feats (remove 'morph
		       syntaxfeatures
		       :test #'(lambda (feat pair)
				 (eq feat (car pair))))))
   
   (case morphfeat
     ;;  any word with a full entry already
     (:none 
      feats)
     ;; verbs
     (:12s123pbase
      (append feats
              (list (list 'vform
                          (build-var 'x
                                     '(base pres)))
                    (list 'agr
                          (build-var 'y
                                     '(1s 2s 1p 2p 3p)))
                    )))
     (:3s
      (append feats 
              (list (list 'vform
                          (build-var 'x
                                     '(pres)))
                    '(agr 3s)
                    )))
     (:ing 
      (append feats
              (list  '(vform ing)
                     )))
     (:past
      (append feats
              (list (list 'vform
                          (build-var 'x '(past))
                          (list 'agr (build-var 'agr '(1s 2s 3s 1p 2p 3p)))
                          ))))
     (:pastpart 
      (append feats
              (list '(vform pastpart)
                    )))
     
     ;;  noun derivatives
     (:sing
      (replace-feat feats 'agr '3s))
     (:plur 
      (replace-feat feats 'agr '3p))
     
     ;;adjective derivatives
     (:er
      (add-er-or-est-feats feats 'er))
     (:est 
      (add-er-or-est-feats feats 'est))
     (-ly
      nil)
     
     (otherwise 
      (parser-warn "Unknown morphological feature ~S in definition of ~S" morphfeat word)
      nil)))
 )

(defun add-load-features (feats)
  (replace-feat 
       (replace-feat 
        (replace-feat feats 'sem '($ abstr-obj))
        'sort 'unit)
      ))
(defun add-er-or-est-feats (oldfeats er-or-est)
  (let ((lf (get-fvalue oldfeats 'lf))
	(op (get-fvalue oldfeats 'comp-op))
	(scale (get-fvalue oldfeats 'pred)))
    (if (null lf) (setq lf (get-fvalue oldfeats 'lex)))
    (cons
     (list 'COMPARATIVE (case er-or-est (er '+) (est 'SUPERL) ))
     (if op (replace-feat oldfeats 'lf (list op scale))
         (replace-feat oldfeats 'lf (list er-or-est lf)))
     )))


(defun getwordDefs (w)
  (gethash w *lexicon-data*))

(defvar *trace-lexicon-building* nil)

#||   obsolete
(defun get-lex-entries (w)
  (if (null *lexicon-data*)
    (build-new-lexicon))
            
  (let* ((defs (getwordDefs w))
         (raw-entries (if defs
                       (mapcar #'(lambda (e)
                                   (make-word-sense-definitions e *synt-table* *lf-ontology*))
                               defs)))
         (kr-senses  (mapcar #'(lambda (e)
                                 (add-kr-info e *lf-ontology* *kr-ontology*))
                             raw-entries))
         )
     kr-senses
))
||# 
        

(defun load-code (&key (silent nil) (kr-in-lexicon *default-kr-in-lexicon*) (kr-boost *default-kr-boost*))
  (excl:load-system :comm :silent silent)
  (excl:load-system :logging :silent silent)
  (ONTOLOGY::load-system :silent silent :lfcode t :krcode t)  
  (excl:load-system :parser :silent silent)
  (excl:load-system :ontology-data-parser :interpreted t)
  (excl:load-system :lexicon :silent silent)
  
  ;; Set various initializations
  (disablegaps)
  (enablesem)
  (traceoff))

(defun load-gram (&key (silent nil) (kr-in-lexicon *default-kr-in-lexicon*) (kr-boost *default-kr-boost*))
  (make-grammar nil)
  (make-lexicon nil)
  (setq *word-tables* nil)
  ;; Load the grammar
  (load-ontology)
  ;; this will init the necessary stuff for type handling
  ;;  (init-type-hierarchy)
  (excl:load-system :lexicon-data :interpreted t)
  (excl:load-system :grammar :interpreted t)
  ;; Load the default scenario
  (setq *kr-in-lexicon* kr-in-lexicon)
  (setq *kr-sense-boost* kr-boost)
  (load-new-scenario user::*default-scenario-directory*))

