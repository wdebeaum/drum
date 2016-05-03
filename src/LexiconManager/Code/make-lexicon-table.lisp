;;;; functions for creating a tsv table containing lexicon-data in a string
;;;; by William de Beaumont

;;;; usage:
;;;; (load "parser")
;;;; :pa
;;;; lxm
;;;; (load "make-lexicon-table.lisp")
;;;; (lexicon->string)
;;;;   => a string containing a comma-separated table representing the lexicon

;;;; to write a file of this, use:
;;;; (with-open-file (s "filename.csv" :direction :output) (format s (lexicon->string)))

;;;; use the following command to remove certain package names and parentheses
;;;; perl -p -e 's/W:://g; s/[\(\)]//g; -i filename.csv

(defvar *heading*
  '(words
    particle
    POS
    LF-parent
    W::sort
    W::mass
    W::atype
    W::comp-op
    W::comparative
    W::fn
    W::subj-map
    W::dobj-map
    W::iobj-map
    W::comp3-map
    W::argument-map
    W::subcat-map
    W::subcat2-map
    feature-list-type
    F::aspect
    F::cause
    F::container
    F::form
    F::gradability
    F::group
    F::information
    F::intentional
    F::locative
    F::measure-function
    F::mobility
    F::object-function
    F::origin
    F::scale
    F::spatial-abstraction
    F::time-function
    F::time-scale
    F::time-span
    F::trajectory
    F::type))

(defun atom-or-list (item)
  (if (listp item)
    (cddr item)
    item))

;; these fields are always NIL (they don't appear in the lexicon):
;; LSUBJ-MAP, COMP-MAP

(defun lex-entry->table-row (le)
  (let* ((words (lex-entry-words le))
         (whole-desc (lex-entry-description le))
	 (pos (car whole-desc))
	 (desc (cdr whole-desc))
	 (whole-sem (cadr (assoc 'W::SEM desc)))
	 (feature-list-type (cadr whole-sem))
	 (feature-list (cddr whole-sem))
	 )
    (append
      (list words)
      (list
        (let ((part (cadr (assoc 'W::PART desc))))
	  (if (and part (not (eql (cadr part) '-)))
	    (cadr (caddr part))
	    'nil
	    )))
      (list pos)
      (list 
        (let ((lf (cadr (assoc 'W::LF desc))))
	  (if (and (listp lf) (equal (car lf) ':*))
	    (cadr lf)
	    lf)))
      (mapcar #'(lambda (x) (atom-or-list (cadr (assoc x desc))))
       '(W::SORT
         W::MASS
	 W::ATYPE
	 W::COMP-OP
	 W::COMPARATIVE
	 W::FN
	 W::SUBJ-MAP
	 W::DOBJ-MAP
	 W::IOBJ-MAP
	 W::COMP3-MAP
	 W::ARGUMENT-MAP
	 W::SUBCAT-MAP
	 W::SUBCAT2-MAP))
      (list (atom-or-list feature-list-type))
      (mapcar #'(lambda (x) (atom-or-list (cadr (assoc x feature-list))))
        '(F::aspect
	  F::cause
	  F::container
	  F::form
	  F::gradability
	  F::group
	  F::information
	  F::intentional
	  F::locative
	  F::measure-function
	  F::mobility
	  F::object-function
	  F::origin
	  F::scale
	  F::spatial-abstraction
	  F::time-function
	  F::time-scale
	  F::time-span
	  F::trajectory
	  F::type))
      )))

(defun row->string (row)
  (if (cdr row)
    (format nil "\"~s\",~a" (car row) (row->string (cdr row)))
    (format nil "\"~s\"~%" (car row))))

(defun lexicon->string ()
  (format nil "~{~a~}"
    (mapcar #'row->string
      (cons *heading*
        (mapcan
          #'(lambda (le-list) (mapcar #'lex-entry->table-row le-list))
          (mapcar
            #'(lambda (word)
	      (handler-case (get-word-lex-entries-from-db word *lexicon-data*)
	        (ONTOLOGYMANAGER::INCONSISTENT-FEAT-SPEC '())
	        (KERNEL::OBJECT-NOT-TYPE-ERROR-HANDLER '())))
	    (lexicon-db-base-forms *lexicon-data*)))))))

