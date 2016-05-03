(in-package "PARSER")

;;
;;  This file contains the code for attaching arbitrary procedural
;;  modifications to entries before they are entered into the chart
;;
;;  There are two forms of attachment: constituent FILTERS and basic CALLS.
;;  FILTERS are invked when the chart parser attempts to add a constituent 
;;  that matches the pattern associated with the filter. The procedure
;;  is called and may modify the constituent before it is added to the chart.
;;  CALLS are explicitly defined within a rule using a user-defined
;;  "category name" and invokes the user defined lisp function. IT may
;;  bind variables and success and fail just as a normal constituent on the
;;  RHS of a rule.

;;  FILTERS
;;  To define a filter, you announce a constituent pattern to the
;;  parser. From then on, every time a constituent that matches the
;;  pattern is about to be added to the chart, your function is called.
;;  Your function may then modify the consituent in any way and return
;;  it and it will be  added to the chart. If your function returns nil,
;;  then the constituent never makes it onto the chart

;;   Here is an example, a function that prints out all
;;    complete NPs that it finds, halves its probability, and
;;    sets a new feature SEEN
;;  EXAMPLE
;;   (defun test (entry)
;;      ;; print out the entry
;;     (Format t "~% Found an np: ~s" entry)
;;     (setf (entry-prob entry) (/ (entry-prob entry) 2))
;;     (setFvalue entry 'SEEN '+)
;;     ;; must return the entry if it is to be added to the chart
;;      entry)

;; (announce '(np (gap -)) #'test)

;; CALLS
;; To define a lisp call, you define an atom that acts as a
;; flag to signal that the interpreter should call a specified lisp function.
;; The arguments to the predicate are passed in as arguments to the
;;   lisp function, which should return binding list (or nil if it fails)

;;   EXAMPLE
;;  e.g., (define-predicate 'trace-args #'(lambda (args)
;;                                      (format t "Args are ~S" args)
;;                                       *success*    ;;  return the successful binding, otherwise rule would fail
;;                                    ))
;;
;;  This would be invoked in the rule and print out the values of the two variables
;;    not otherwise affecting the parse
;;      ((S (AGR ?agr) (SEM ?sem))
;;           <-
;;              (NP (AGR ?arg))
;;              (VP (AGR ?agr) (SEM ?sem))
;;              (trace-args ?agr ?sem))



;;  FUNCTIONS THAT MAINTAIN THE ATTACHMENT DATABASES

(let ((filters)
      (predicates))
  
  (defun init-attachments nil
    (setq filters (make-hash-table :test 'equal :size 10))
    (setq predicates nil))
  
  ;;   ANNOUNCE
  
  ;;  PATTERN must be parsable as a constituent pattern (as in grammar rules)
  ;;  FN is a lisp function that takes a chart entry
  ;;  as its one argument and returns a modified entry for further
  ;;  processing by the parser.

  ;; As far as I can tell, this builds up an attachment list where the
  ;; final attachment added is the first one in the list.
  ;; Doc Mar 30 2005 ScS
  
  (defun announce (pattern call)
    (let*
	((pat (verify-and-build-constit pattern (list pattern call) nil))
	 (cat (constit-cat pat)))
      
      (if pat
	  (setf (gethash cat filters)
	    (cons (list pat call)
		  (gethash cat filters))))))

  ;; As far as I can tell, this processes the attachment list so that
  ;; the final attachment in the attachment list is added as the first
  ;; thing in the calls list.  So, in conjunction with the previous
  ;; code, when these actually get called, the attachments will get
  ;; returned from find-functions in the order that they were added
  ;; as attachments.
  ;; Doc Mar 30 2005 ScS
  
  (defun find-functions (constit)
    (let ((calls nil))
      (mapcar #'(lambda (item)
		  (let ((pattern (car item))
			(fn (cadr item)))
		    (if (constit-match pattern constit)
			(setq calls (cons fn calls)))))
	     (gethash (constit-cat constit)  filters))
    calls))

  (defun filters nil
    filters)
  
  (defun attached-predicates nil
    predicates)
  
  (defun define-predicate (name function)
    (setq predicates (cons
		      (cons name function)
		      predicates)))
  
  (defun check-for-defined-predicate (name)
    (cdr (assoc name predicates)))
  
  (defun defined-predicates nil
    predicates)
  
  
  ) ;; end scope of filters

(init-attachments)

;; PROCEDURAL-FILTER
;; =================

;; Given a chart entry, this searches for functions which have been
;; registered as attachments (see Trips-Parser/attachments.lisp) to
;; act on entries of this type.  Each such function is run over the
;; entry.  A list is built up of the (non-nil) results.

;; If the list is empty (that is, if no procedures were found to run,
;; or if all procedures found returned 'nil results), then the original
;; entry is returned.  If the list contains any members, then the
;; first one is arbitrarily chosen to be the new entry.

;; LET THE USER BEWARE
;; (a)  The entry returned by the procudural function registered is
;;      passed right back to code that sticks it on the chart/agenda.
;;      nothing checks to make sure that what is returned is actually
;;      a valid entry, so make sure that when you write an attachment,
;;      it returns a valid entry.
;; (b)  It is up to whoever is setting up the attachments to ensure
;;      that the priority ordering of attachments is correct.  As far
;;      as I can tell (see above), attachments will get called in the
;;      order that they are added.  So, if the first attachment registered
;;      applies, then if it returns a non-nil argument, then it will be
;;      processed regardless of whatever other attachments do.
;; Doc Mar 30 2005 ScS


;; Incremental Procedural-Filter
;; -----------------------------
;; Mar 30, 2005 ScS

;; Procedural-filter has been modified (along with add-entry-to-agenda)
;; so that when a procedural filter kicks in, it is possible to acutally
;; delay the entry of the constituent until later.  If this function
;; returns an entry to add-entry-to-agenda, then the entry will be added.
;; If this function returns a 'nil, then the entry will not be added.

;; This function will return a 'nil if the first non-nil attachment
;; return value is the constant value 'DELAY-ENTRY.  Otherwise it will
;; return the first value on the list, or the entry it is passed, as
;; before.  An attachment that wants to delay the entry of a constituent
;; should pass back 'DELAY-ENTRY as its return value (and make sure that
;; its continuation eventually calls add-entry-to-agenda-continued with
;; appropriate parameters).

;; Implementation Note:  this should not change the current functioning
;; of the code at all.  The only difference is that the function will
;; now return 'nil to add-entry-to-agenda if 'DELAY-ENTRY is returned.

;; DELAY-CONSTITUENT-ENTRY
;;
;; It would be nice if there were a function here that would handle the
;; details of delaying a constituent entry for the attachment, so that
;; it doesn't need to know the details of sending back 'DELAY-ENTRY or
;; calling add-entry-to-agenda later.  For now, the attachments need to
;; be "smart".

(defun procedural-filter (entry)
  (let*	((results nil))
    (mapcar #'(lambda (fn)
		(let ((res (funcall fn entry)))
                (if res (setq results (cons res results)))))
	    (find-functions (entry-constit entry)))
	;; If the first non-nill entry is 'DELAY-ENTRY, then a nil is
	;; returned, which will cause add-entry-to-agenda to not add the
	;; entry (or otherwise process it).  If you return 'DELAY-ENTRY,
	;; you'd better call add-entry-to-agenda-continued later to process
	;; it, or you're in trouble;  that entry will be lost!
	(if results
		(if (equal (car results) 'DELAY-ENTRY) nil (car results))
	  entry
	  )
	
	)
  )

;; OLD CODE:      (if results (car results) entry)))
;; Mar 30 2005 ScS

;;
;;--------------

;;   CONVENIENCE FUNCTIONS
;;  This aren't necessary, but make it easier for the novice writing
;;  attached functions.

;; Retrieve a feature value
(defun getFvalue (entry feature)
  (get-value (entry-constit entry) feature))

;; Set/replace a feature value
(defun setFvalue (entry feature value)
  (setf (entry-constit entry)
    (replace-feature-value (entry-constit entry) feature value)))

;; set the probability
(defun setProbValue (entry newprob)
  (setf (entry-prob entry) newprob))


   
		    
		    
