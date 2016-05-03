(in-package "LEXICONMANAGER")
;;;;
;;;; make-templates.lisp
;;;;

;;
;; The file for handling syntax.
;; Reads in the template file and makes hash table indexed by template name.
;;

;;
;; Each template makes a corresponding structure with
;; LF's - to be filled after the lexicon is compiled
;; A-list indexed by synt.argument type
;; Defaults for substitution - a-list indexed by parameter name.

;;
;; Each synt.argument is represented by the structure
;; that contains 
;; Corresponding slot in the semantics file
;; Variable name
;; type (restriction/phrase to be susstituted in the vocabulary)
;; Syntax restriction
;; default value for substitution
;;

(defun retrieve-template (templ-name)
  (gethash templ-name  (lexicon-db-synt-table *lexicon-data*))
  )

;; Takes a template, parses it and inserts it in the hash table
;; returns the parsed version, or nil if there were any problems
(defun insert-template (templ synt-table)
  (let ((templ-str (parse-template templ)))
    (cond
     ((null templ-str) nil)
     ((gethash (syntax-template-name templ-str) synt-table)
      (lexiconmanager-warn "Template ~S defined twice. Ignoring second definition" 
                   (syntax-template-name templ-str))
      nil)
     (t
      (sethash (syntax-template-name templ-str) templ-str synt-table)
      templ-str
      ))
    ))

;;
;; Parse the template. Return a corresponding structure
;; name is template name
;; restrictions is a-list indexed by restriction, consed with restrictions structs
;; returns nil if the template had syntax problems
;; includes ocndition handler to signal mistakes
;; will need to write a better handler!

(defun parse-template (template)
  (let ((name (first template))
	(restrs (get-arg-value-list 'arguments (cdr template)))
	(syntax (get-arg-value-list 'syntax (cdr template)))
	(resrestrs nil)
	)
    (setf resrestrs
      (handler-case (mapcar #'parse-syntax-restriction restrs)
	(invalid-templ (err) 
	  (lexiconmanager-warn "Invalid template ~S:~%  ~A~%" name err)
	  nil
	  )))
    (make-syntax-template :name name :mappings resrestrs :syntax syntax)
    ))


;;
;; Parses an entry and returns in consed with its name, ready to insert in the a-list
;; !!! Substitution on top level only !!!
;; Returns the name(lsubj etc) consed with the proper structure
;; !!!!!!!! Will have to add checks on parameter coccurrence
;;

(defun parse-syntax-restriction (restr)
  (let* (
	 (name (first restr))
	 (synt (second restr))
	 (slot (third restr))
	 (optparams (cdddr restr)) ;; optional parameters - optional, map-only
	 (paramname nil)
	 (default synt)
	 (required nil) 
	 (defaultvars `((w::case ,(right-var name '? 'w::case)) 
			(w::agr ,(right-var name '? 'w::agr))
			(w::var ,(right-var name '? 'w::var))
			(w::lex ,(right-var name '? 'w::lex))
			(w::sort ,(right-var name '? 'w::sort))
			(w::lf ,(right-var name '? 'w::lf))
			;;			(class ,(right-var name '? 'class))
			))
	 )

;; Need to check for problems with optional parameters.
    (check-optional-parameters optparams)
;; now check if we have a parameter here and parse it as needed
    (if (eql (first synt) :parameter)
	(progn
	  (setf paramname (second synt))
	  (setf default (get-arg-value :default (cddr synt)))
	  (setf required (get-arg-value-list :required (cddr synt)))
	  )
      ;; if we don't have the parameter, then all features we have declared are required
      ;; we just parse taking into account that there may be a variable there
      (setq required (if (eql (first synt) '?) (cddr (third synt)) (cddr synt)))
      )
    ;; now we put in the default vars, but only if they are not already defined in the required features
    (setf required (merge-in-defaults required defaultvars))
    ;; Now make a structure and cons it with the name
    (cons
     name
     (make-synsem-map
      :name name
      ;; we make all slot names into lists for uniformity
      :slot slot
      :paramname paramname
      :default default
      :required required
      :optional (memberp 'optional optparams)
      :maponly (memberp 'maponly optparams)
      ))
    ))
  
;; Signalls an error to the higher level
(defun check-optional-parameters (params)
  (let ((mytry nil))
    (dolist (el params)
      (when (not (or
		  (eql el 'optional)
		  (eql el 'maponly)))
	;;	(format nil "Incorrect optional parameter ~S~%" el))	
	(setf mytry 
	  (make-system-condition
	   'invalid-templ
	   :format-control "Incorrect optional parameter specification ~S"	
	   :format-arguments (list el)
	   ;;	       :format-str "Incorrect optional parameter specification ~S"))
	   ))
	(signal mytry)
	))))








