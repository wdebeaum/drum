;;;;
;;;; scenario.lisp: Parser scenario routines
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 10 May 1997
;;;; Macros are used here to get the quoting required by Lisp that
;;;; isn't in the neutral format scenario files.
;;;;

(in-package "PARSER")

(declaim (special *lf-ontology*))
(declaim (special *kr-ontology*))
(declaim (special *scenario-object-files*))


;;;
;;; These are used in the scenario objects file.
;;;

(defmacro define-scenario (&rest args)
  (declare (ignore args)))

(defmacro define-map (&rest args)
  (declare (ignore args)))

(defmacro define-place (&rest args)
  `(apply #'scenario-define-place ',args))

(defmacro define-connection (&rest args)
  `(apply #'scenario-define-connection ',args))

(defmacro define-connection-status (&rest args)
  (declare (ignore args))) 

;;(defmacro define-class (&rest args)
;;    `(apply #'scenario-define-class ',args))

(defmacro define-object (&rest args)
  `(apply #'scenario-define-object ',args))

;;(defmacro define-type (&rest args)
;;  `(apply #'scenario-define-type ',args))

;;;
;;; This is how a new scenario gets loaded
;;;

(defun load-new-scenario (dirname)
  "Loads a new scenario from the given DIRNAME."
  ;; Make sure we're in same package now as at load-time...
  (let ((*package* (find-package "PARSER")))
    ;; Load new object info
    (load-domain-ontology dirname)
    (mapc (lambda (x) (load-scenario-file dirname x)) *scenario-object-files*)
    ))
  

(defun load-scenario-file (dirname filename)
  (let ((pathname (make-pathname :directory dirname
				 :name filename :type "txt")))
    (format *trace-output* "~&Parser: loading scenario ~A file ~S~%"
	    filename pathname)
    (when (not (load pathname :verbose nil :if-does-not-exist nil))
      (format *error-output* "Parser: ~A file doesn't exist: ~A~%"
	      filename pathname))))

(defun load-scenario-directory (dirname)
  (let ((pathname (make-pathname :directory dirname
				 :name "load-data" :type "lisp")))
    (format *trace-output* "~&Parser: loading scenario data file ~S~%"
	    pathname)
    (when (not (load pathname :verbose nil :if-does-not-exist nil))
      (format *error-output* "Parser: ~A file doesn't exist~%"
	      pathname))))

(defun load-domain-ontology (dirname)
  ;;  (initialize-lf-kr-interface)
  ;;  (load-scenario-file dirname "classes")
  ;; (load-scenario-file dirname "operators")
  ;; (load-scenario-file dirname "predicates")
    ;;  (load-scenario-file dirname "H-transforms")
  ;;(load-scenario-file dirname "predicate-transforms")
  ;;(load-scenario-file dirname "frame-transforms")
  ;;(load-scenario-file dirname "frame-predicate-transforms")  
  
  (load-scenario-directory dirname)
  (add-kr-to-type-hierarchy *kr-ontology*)
  ;;  (integrate-kr *lf-ontology* *kr-ontology*)
  )


(defun load-type-file (typepathname)
  (or (load typepathname :verbose nil :if-does-not-exist nil)
      (format *error-output* "parser: types file doesn't exist: ~A~%"
	      typepathname))
  
  )
;;;
;;; This is for incremental scenario changes (not everything can be changed)
;;;

(defun modify-scenario (&key add delete)
  "Nothing that can be changed matters to the parser."
  (declare (ignore add delete)))

;;;
;;; Functions and variables used in the processing of scenario files
;;;

(defun scenario-define-place (obj &key name type &allow-other-keys)
  (when name
    (make-name-entry obj name type)))

(defun scenario-define-connection (obj &key name type &allow-other-keys)
  (when name
    (make-name-entry obj name type)))

(defun scenario-define-object (obj &key name type &allow-other-keys)
  (when name
    (make-name-entry obj name type)))

;;(defun scenario-define-class (class &key isa slots &allow-other-keys)
;;  (add-kr-type class :isa isa :slots slots )
;;  )

;;(defun scenario-define-type (type &key (parent nil) (semantics nil) (arguments nil) &allow-other-keys)
;;  (add-linguistic-type type *lf-ontology* *kr-ontology* :parent parent :semantics semantics :arguments arguments)
;;)

;;
;; gf: Should this be elsewhere?
;;
(defun make-name-entry (object name class)
  "Adds a lexicon entry for OBJECT with NAME and CLASS, where NAME
may be a string."
  (cond
   ((consp name)
    (mapcar #'(lambda (n) (make-name-entry object n class))
            name))
   ((not (stringp name))
    (parser-warn "name is not a string: ~S" name))
   (t
    ;; Else convert to atom or list of atoms
    (setq name
      (let ((name-atoms (read-from-string (format nil "(~A)" name))))
	(cond ((eql (length name-atoms) 1)
	       (first name-atoms))
	      (t
	       name-atoms))))
;;;    ;; If not already in lexicon...
;;;    (unless (some
;;;	     #'(lambda (entry)
;;;		 (eq (constit-cat (lex-entry-constit entry)) 'name))
;;;	     (retrieve-from-lex name *default-grammar*))
            
      (case class
	((ont::street ont::road ont::highway)
	 (add-street-to-lexicon name class))
	(t
	 ;;	 (format t "will add name to lexicon")
	 (add-name-to-lexicon (make-name-entry-with-sem name class))))
      ;;      )
      )))


(defun add-name-to-lexicon (entry)
;;;  (augment-lexicon
;;;   (expand 'name (list :node '()
;;;                       (list (append (list :leaf (car entry) 
;;;					   (if *kr-in-lexicon* *kr-sense-boost* 1.0))
;;;				     (cdr (second entry))))))))
;;;  (let* ((word (car entry))
;;;	 (lex (cgrammar-lex *default-grammar*))
;;;	 (olddef (gethash word lex))
;;;	 )
;;;    (setf (gethash word lex) (cons (second entry) olddef))
;;;    ))
  (let* ((word (word-sense-definition-word entry))
	 (rest (word-sense-definition-remaining-words entry))
	 (lex (lexicon-db-word-table *lexicon-data*))
	 (olddef (gethash word lex))
	 (rname (word-sense-definition-name entry))
	 )
    (setf (gethash word lex) 
      (cons
       (list :none rname (make-vocabulary-entry
			  :name rname	     
			 :word word
			 :remaining-words rest
			 :pos 'name
			 :senses (list entry)
			 ))
       olddef))
    ))
  
(defun make-name-entry-with-sem (name class &optional fname)
  "Defines a CLASS entry unless it is already there"
  (multiple-value-bind (word part rest)
      (parse-word-spec name)
    (format t "Name:~S Class: ~S ~%" name class)
    (let* ((transform (get-transform-for-class class *lf-ontology* *kr-ontology*))
	   (type (get-type-for-class class *lf-ontology* *kr-ontology*))
	   (sem (get-sem-for-class-type class type *lf-ontology* *kr-ontology*))	 
	   (full-name (or fname name))
							       
	   (entry (make-word-sense-definition
		   :name (gen-id (if rest (cons word rest) word))
		   :pos 'name
		   :lf type
		   :sem sem
		   :word word
		   :remaining-words rest
		   :pref -1 ;; (if *kr-in-lexicon* *kr-sense-boost* 1.0)
		   :transform transform
		   :specialized t
		   :kr-type class
		   :syntax `((name +) (full-name ,full-name) (class ,class) (agr 3s))
		   ))
	   (fword (if (consp word) (first word) word))	    
	   )
      (when (AND type name)
	(add-kr-coercions class entry *lf-ontology* *kr-ontology*)
	;;	(make-lexicon-entry word entry))
	entry)
      )))

(defun get-sem-for-type (type)
  (let ((lfdescr (get-lf-definition type *lf-ontology*)) (sem nil))
    (cond
     ((null lfdescr) '?sem)
     ((null (lf-description-sem lfdescr)) '?sem)
     (t (setq sem (lf-description-sem lfdescr))
	(cons '$ (cons (feature-list-type sem) (feature-list-features sem)))
      ))
    ))

  
(defun add-street-to-lexicon (s class)
  "Defines a street entry unless it is already there"
  (mapcar #'add-name-to-lexicon (make-street-entries s class))
  )

(defun make-street-entries (s class)
  "Defines a list of entries associated with a street and its possible short names unless they are already there"
  (let* ((entries (retrieve-from-lex s *default-grammar*))
	 ;; slf gets to be east_ridge_road
	 (sname (reduce (lambda (x y) (read-from-string (format nil "~S_~S" x y))) s))
	 (sentry (make-name-entry-with-sem s class sname))
	 )
    (unless (and entries (some #'(lambda (w) (eq (constit-cat (lex-entry-constit w)) 'route)) entries))
      ;; for a street we make 2 entries, with and without the type specifier (rd,street etc)
      ;; the "if" is there because we need to make a compound foe multi-word street names
      ;; and simple names for regular names
      (if (> (length s) 2)
	  ;; define 2 composite names: e.g. (East River Rd) and (East River)
	  (list sentry (make-name-entry-with-sem (butlast s) class sname))
	;; else - a 2-word name, so a composite and a simple name
	;; e.g. (Elmwood Ave) and Elmwood
	(list sentry (make-name-entry-with-sem (first s) class sname))
	))
    ))




