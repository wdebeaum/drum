;;
;;; printing.lisp : Printing Parser data structures
;;;
;;; Author:  James Allen <james@cs.rochester.edu>
;;;

;;; Time-stamp: <Wed Apr 19 10:39:18 EDT 2017 jallen>

(in-package "PARSER")

(defvar *print-package-flag* nil) ;; if non-nil, the terminal output routines will not suppress package prefixes
(defvar *simplify-numbers* t)

;;************************************************************************
;;
;;   PRINTING FUNCTIONS
;;

;;************************************************************************
;;
;;   PRINTING and ACCESSING THE CHART
;;


(defun show-chart (&optional feats start end &key constits)
   (format t "~%")
   (mapcar #'(lambda (x) 
	       (show-entry-with-name x feats))
	   	(get-constits-from-chart start end :constits constits))
   (values))

(defun find-spanning-constits-in-chart (constits start end)
  "Find constituents that span between start and end"
  (let* ((cs (mapcar #'(lambda (x)
			 (util::convert-to-package x *w-package*))
		     (if (symbolp constits) (list constits) constits)))
	 (candidates (get-constits-from-chart start end :constits cs)))
    (remove-if-not #'(lambda (x)
		       (and (eq start (entry-start x)) (eq end (entry-end x))))
		   candidates)))

(defun find-spanning-constit-names-in-chart (constits start end)
  (mapcar #'entry-name
	  (find-spanning-constits-in-chart constits start end)))

(defun find-nodes-in-trees-rooted-at (clist)
  "finds names of all subnodes of trees woth roots in CLIST"
  (remove-duplicates
   (flatten
    (mapcar #'(lambda (c)
		(cons c (get-subnodes c)))
	    clist))))


;;------------------------
;; show-best
;;
;; print just those constituents that span the input
;;
(defun show-best (&optional features start end)
  (let ((start (if start start 0))
        (end (if end end (get-max-position))))
    (format t "~%")
    (if (consp features)
      (mapcar #'(lambda (x)
                  (show-entry-with-name x features))
                  (cadar (find-best-paths  start end 1)))
      (mapcar #'show-entry-with-name (cadar (find-best-paths start end 1))))
    (values))
  )

;;------------------------
;; get-best
;;
;; like (show-best), but return best for parse-tree viewer.
;;
(defun get-best (&optional start end)
  (let ((start (if start start 0))
        (end (if end end (get-max-position))))
    (mapcar #'get-entry-with-name (cadar (find-best-paths start end 1)))
    ))

;;------------------------
;; show-answers
;;
;; Print out the complete structure of the best parse
;;
(defun show-answers (&optional features &key (start nil) (end nil) (nth 1))
  (print-parse :syntax :start start :end end
	       :feats (util::convert-to-package features *w-package*)
	       :stats '(:pos :prob :score :Rule :id) :nth nth))

;;------------------------
;; get-answers
;;
;; Like (show-answers), but put parse into a list
;;
(defun get-answers (&optional start end)
  (get-parse :syntax :start start :end end :stats '(:pos :prob :score
							 :rule :id))
  )

;;-----------------------------------------------------
;;  GET-PARSE
;;  The main workhorse for extracting information about the parse

(defun get-parse (info &rest args)
  "Returns a list representation of parse tree of best interpretation 
   INFO is one of the following:

              :syntax - syntactic treein
              :lf - the logical form
             
   e.g., SHOW-ANSWERS is implemented as (get-parse :syntax :start start :end end :stats '(:pos :score :rule-id))

  Then there are a number of optional keyword arguments as follows:

       :start - the starting position in the chart to explore (default is 0)
       :end - the end position in the chart to define the search range (default is max position)
       :feats - a list of the features to be included in the syntactic tree (default is all features) (:syntax option only)
       :stats - a list of optional information that can be returned
              :pos - include the positions from the chart for each object
              :score- include the score for each object
              :rule - include the rule id that produced each objects (:syntax only)
              :id - include the consituent id from the chart   (:syntax only)   
       :numb  - the number of parses desired 
       :NTH - a number requesting information for the n'th best parse in the current chart. This function returns NUMB interpretations starting at the NTH parse"

  (let* ((start (or (find-arg args :start) (get-min-position)))
	 (end (or (find-arg args :end) (get-max-position)))
	 (feats (find-arg args :feats))
	 (stats (find-arg args :stats))
         (xtra (find-arg args :xtra))
	 (numb (or (find-arg args :numb) 1))  ;; default is find one parse
         (nth (or (find-arg args :nth) 1)))
     (case info
       (:syntax (mapcar #'(lambda (x)
                            (build-tree x feats stats nil))
                        (cadr (nth (- nth 1) (find-best-paths start end (or nth 1))))))
       (:lf (let* ((analyses (last (getanalyses (get-current-channel) (+ numb (- nth 1)) nil) numb))
		   (filtered-analyses (mapcar #'(lambda (x) (filter-feats x feats)) analyses))
		   (ans (mapcar #'(lambda (x) (build-new-lf x xtra)) filtered-analyses)))
	      (if *simplify-numbers*
		  (mapcar #'simplify-numbers ans) ans))
              )
       (otherwise (parser-warn "ERROR: unknown first argument ~S, Must be :syntax, :lf or :kr" info)))))

;; Do a shortest-path algorithm for best constit-sequence to cover input, where constituents have a fixed cost.
;;  Costs  
;;
(defvar *cost-table* '((w::PUNC 4)  ;; Puncs should be attached to their utts
                       (w::UTT 1)  ;; making UTT one prefers sequences with minimal number of UTTs
                       (w::NP 3) (w::PATH 3) ;; These are rare now, as most major consists become UTTs
                       (w::ADVBL 3) (w::ADJP 3)  ;; other high level constits
		       ;;  the speech acts get individual scores
		       (w::SA_PRED-FRAGMENT 2)
		       ))

(defun customize-cost-table (values)
  (setq *cost-table* (append values *cost-table*)))

(defun constit-cost (c)
  (let ((cc (constit-cat c)))
    (if (eq cc 'w::utt)
	;; utts are now scored by speech-act
	(let* ((sa (get-value c 'w::lf))
	       (score (if sa (cadr (assoc (get-value sa 'w::class) *cost-table*)))))
	  (or score (cadr (assoc 'w::utt *cost-table*))))
	;; not an utt, use table
	(cadr (assoc (constit-cat c) *cost-table*)))))	

(defvar *paths*)    ;; an array to store the best path to each position
(defvar *threshold-factor* 1)  ;; This is used to compute a threshold to stop search when the cost of a path
                               ;;     exceed the best found so far by the specified factor. 
(defvar *threshhold*)   ;; this records threshold, initially set arbitrarily high

(defun entry-cost (e)
  "Returns the cost of traversing a constituent, defaulting to 5 if not in *cost-table*"
  (if (entry-p e)
      (let* ((prob (entry-prob e))
	     ;;(score (calculate-score e))
	     (c (entry-constit e))
	     (cat-cost (constit-cost c)) ;;(cadr (assoc (constit-cat c) *cost-table*)))
	     (constit-cost (if (> prob 0) ;;(> score 0))
			       (/ (or cat-cost 5) prob)
			       ;;(/ (or cat-cost 5) score)
			     100000)))
	;;	(format t "~%COST of ~S with prob ~S and cat-cost ~S is ~S" (constit-cat c) (entry-prob e) cat-cost constit-cost)
	constit-cost
	)
    (progn
      (parser-warn "Bad arg passed to entry-cost: ~S" e)
      100000)))

;;  This best path algorithm uses an array, each element storing a priority queue of all
;;   paths found so far to this location. We keep them all as we may need to find several solutions,
;;   or the N'th best solution
;;  This algorithm assumes that all valid paths end at the same final position. Thus a lattice has
;;    to be preprocessed to insert a "noise" constituent to extend a path to the end position.

(defun find-best-paths (start end N)
  "Do the shortest path search: returning cost and path of the N best solutions. We first compute the min cost path
  to each position, then we scan backwards to find the best - which combines cost and amount of input covered"
  ;;  set threshhold factor based on the size of N
  (setq *threshold-factor* (+ 1 (* .33 (max 0 (- N 1)))))   ;; it is 1.0 for 1 interp, 1.5 for 2nd, 2.0 for 3rd, etc.
  (when (> end 0)
    (let
	((*paths* (make-array (list (+ end 1)) :initial-element nil)))
      
      (setf (aref *paths* 0) (list (list 0 nil)))   ;; set first position to 0
      (multiple-value-bind 
	  (newstart *threshhold*)
	  (find-parameters start end)
	(setf (aref *paths* newstart) (list (list 0 nil)))
	(do ((i newstart (+ i 1))) ((> i end))
	  (extend-from-position i end))
	;;      (print "Current paths")
	;; (print *paths*)
	(construct-N-best (aref *paths* end) n)
       	#||(let* 

	    ((ans (nth (- N 1) (aref *paths* end)))
         )
         (list (car ans)
               (reverse (cadr ans))))
	    ||#
      )
    )))

(defun construct-N-best (solns N)
  "build a list of N solutions of form (cost path)"
  (when (> N 0)
    (cons (list (caar solns) (reverse (cadar solns)))
	  (construct-N-best (cdr solns) (- N 1)))))

(defun extend-from-position (i end-position)
  "Check all extensions from position i and update any new best paths"
  (let* ((paths (aref *paths* i)))
   (mapcar #'(lambda (path)
                (let 
                  ((cost-so-far (car path))
                   (path-so-far (cadr path)))
                  ;;(format t "~%Trying to extend from position ~S. PATH so far is ~S; cost is ~S" i (mapcar #'entry-name path-so-far) cost-so-far)
                  ;; If lowest cost to position i is greater than best to end so far we can skip
                  (when (< cost-so-far *threshhold*)
                    (let ((entries (get-entries-by-position nil i)))
                      (mapcar #'(lambda (e)
                                  (try-to-extend e cost-so-far path-so-far end-position))
                              entries)))))
            PATHS)))

(defun try-to-extend (entry cost-to-start path-to-start end-position)
  (let* ((c-cost (entry-cost entry))
         (new-end (entry-end entry))
         (cost-to-end  (if (aref *paths* new-end)
                         (caar (aref *paths* new-end))
                         10000))
         (cost-of-new-path (+ cost-to-start c-cost)))
    ;;    (if (eql (constit-cat (entry-constit entry)) 'w::utt)
    ;;   (format t "~%For entry ~S ending at ~S cost is ~S~%" (entry-name entry) new-end cost-of-new-path)
    ;;)
    ;; only continue if new path costs less than some factor of the best so far 
    (if (and (<  cost-of-new-path (* *threshold-factor* cost-to-end)) (< cost-of-new-path *threshhold*))
      (let
        ((new-path (cons entry path-to-start)))
       ;;(format t "~%Adding path to position ~S with ~S with cost ~S. threshold is ~S. end is ~S" 
       ;;       new-end (mapcar #'(lambda (e) (entry-name e)) new-path) cost-of-new-path *threshhold* end-position)
        (setf (aref *paths* new-end)
              (insert-in-priority-queue cost-of-new-path new-path (aref *paths* new-end) 0))
        (when (= new-end end-position)
          ;;(format t "~%Updating threshhold to ~S" cost-of-new-path)
          (setq *threshhold* (* *threshold-factor* cost-of-new-path)))))))



(defvar *max-depth* 100)

(defun insert-in-priority-queue (cost el q depth)
  (cond
   ((or (null q) (> depth *max-depth*)) (list (list cost el)))
   ((< cost (caar q))
    (cons (list cost el) q))
   (t 
    (cons (car q)
	  (insert-in-priority-queue cost el (cdr q) (+ depth 1))))
   ))


;; Code to initialize the search parameters (start position and threshhold)

(defun find-parameters (start end)
  "This does a couple of things. First it finds the starting point of the utterance, which will
usually not be 0 for speech. Also it finds one path quickly in order to set the upper-bound on path cost"
  (declare (ignore start))
  (let*
    ((newstart 
     (find-first-non-null 0 end))
     (threshhold
      (quick-find newstart end 0)))
    (values newstart (* 1.01 threshhold))))

(defun find-first-non-null (i end)
  (if (or (get-entries-by-position nil i) (>= i end))
    i
    (find-first-non-null (+ i 1) end)))

(defun quick-find (start end cost-so-far)
   (let*
    ((extensions (get-entries-by-position nil start))
     (longest (car (sort (copy-list extensions) #'> :key #'entry-end))))
    (if longest
      (let*
        ((end-so-far (entry-end longest))
         (cost (entry-cost longest)))
        (if (>= end-so-far end)
          (+ cost-so-far cost)
          (quick-find end-so-far end (+ cost-so-far cost))))
      ;; we hit a dead end, return a high number as we can't find a threshhold
      10000)))
    
;;=========================
;;    Building the lisp-style  tree 

(defun build-tree (entry feats stats bindings)
  "Builds a list form of the tree rooted at the ENTRY, showing the feature
     values in FEATS and the statistics in STATS"
  (let ((tree (get-tree entry bindings)))
    (bt tree feats stats)))

(defun bt (tree feats stats)
  (when tree
    (if (cdr tree)
	(let ((subs (mapcar #'(lambda (n) (bt n feats stats)) (cdr tree))))
	  (append
	   (build-entry-with-name (car tree) feats stats) (list :sub subs)))
      (build-entry-with-name (car tree) feats stats))))
     
(defun build-entry-with-name (entry feats stats)
  "Builds the list form of an entry (not including its subconstituents)"
  (let ((c (insert-positional-info 
	    (insert-input-feature (entry-constit entry)) entry)))
    (append
     (list (constit-cat c))
      (list :feats (simplify-embedded-constits (extract-features (constit-feats c) feats) feats))
      (extract-stats entry stats))))

(defun extract-features (feat-list feats)
  "removes the features we don't want to see. The default is remove the subconstituent
     features"
  (let ((ans
         (if feats
           (remove-if-not #'(lambda (x) (member (car x) feats)) feat-list)
           (remove-if #'(lambda (x) (member (car x) '(1 2 3 4 5 6 7 8 9))) feat-list))))
    (if ans (constit-to-list ans))))

(defun simplify-embedded-constits (value feats)
  "keeps only desired features (i.e., ones is FEATS) in any embedded constits in the value"
  (if (Consp value)
    (if (eq (car value) '%)
      (list* '% (second value) (simplify-embedded-constits (extract-features (third value) feats) feats))
      (mapcar #'(lambda (v)
                  (simplify-embedded-constits v feats))
              value))
    value))

(defun extract-stats (entry stats)
  (when stats
    (list :stats
          (mapcan #'(lambda (stat)
              (list stat (case stat
                                 (:pos (list (entry-start entry) (entry-end entry)))
                                 (:prob (entry-prob entry))
				 (:score (calculate-enTry-Score entry))
                                 (:rule (entry-rule-id entry))
                                 (:id (entry-name entry))
                                 (otherwise 'ERROR-UNKNOWN-STAT)
                                 )))
          stats))))

(defun filter-feats (analysis feats)
  "If FEATS is specified, we must remove unwanted features from embedded constits"
  (if (null feats)
    analysis
    (filter-features analysis feats)))

(defun filter-features (analysis feats)
  "Remove unwanted features from embedded constituents"
  (if (or (null analysis) (not (consp analysis)))
    analysis
    (if (eq (car analysis) '%)
      (cons '% (cons (second analysis)
                     (remove-if-not #'(lambda (f) (member (car f) feats)) (third analysis))))
      (cons (filter-features (car analysis) feats)
            (filter-features (cdr analysis) feats)))))

(defun convert-to-frame-format (analysis)
  "Converts feature structure format into frame format"
  (if (or (null analysis) (not (consp analysis)))
    analysis
    (if (eq (car analysis) '%)
      (convert-constit-to-frame analysis)
      (cons (convert-to-frame-format (car analysis))
            (convert-to-frame-format (cdr analysis))))))

(defun convert-constit-to-frame (con)
  (cons (second con)
        (mapcan #'(lambda (x)
                    (list (keywordify (car x)) (second x)))
                (cddr con))))

(defun print-parse (&rest args)
  (let ((ans (apply #'get-parse args))
	(stream nil)
	)
    (case (car args)
      (:syntax
       (pprint-logical-block (stream nil)
        ;; (case (car args)
	   ;;(:syntax
	    (mapc #'(lambda (tree)
			      (pprint-logical-block (stream nil)
			        (terpri stream)
				;;(pprint-newline :mandatory stream)
			        (print-tree tree stream))
			      )
		          ans))
	   ;;(:lf (print-lf ans))
	   )
         ;;))
      (:lf
       (pprint ans)))
    (values nil)
    ))

(defvar *id-seen* nil)

(defun print-lfs (&rest args)
  (let* ((*id-seen* nil)
	 (parses (eval (list* 'get-parse :lf args))))
    (mapc #'print-lf parses)))

(defun print-lf (parse)
  (if (eq (car parse) 'compound-communication-act)
      (progn (format t "~%COMPOUND ACT:")
	     (mapcar #'print-single-utt (find-arg-in-act parse :acts)))
      (print-single-utt parse)
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; swift -- some utility functions for displaying the transformed lf
;;

(defvar *showlf* nil)

(defun show-lfs (lfs)
  (when lfs
    (when (listp lfs)
      (setq *showlf* (push (car lfs) *showlf*))
      (mapcar #'(lambda (x) (show-lfs x))
	      (cdr lfs))))
  (reverse *showlf*))

(defun return-lf (&rest args)
  "like (lf) but returns the result without printing it"
  (let* ((*showlf* nil)
	 (*id-seen* nil)
	 (parse (eval (list* 'get-parse :lf args)))	 
	 (ans (find-arg-in-act parse  :terms))
	 (root (find-arg-in-act parse :root))
	 (result (sort-lfs root (if *kr-type-info-desired* (extract-LFs+wnsense ans *kr-type-info-desired* t)
				    (extract-lfs ans)))))
    (show-lfs result))
  )
  
(defun show-transformed (&optional all &rest args)
  "shows the result of the transforms applied to the lf"
  (if all
      (om::transform (return-lf))
    (first (om::transform (return-lf)))
    )
  )

(defvar *show-words* t)

(defun print-single-utt (parse)
  (let* ((ans (find-arg-in-act parse  :terms))
	 (root (find-arg-in-act parse :root))
	 (result (sort-lfs root (extract-lfs ans))))
    (if *show-words* (format t "~%~%;;;;; ~S" (find-arg-in-act (find-if #'(lambda (x)
									    (eq (find-arg-in-act x :var) root))
									ans)
							       :input)))
    (print-lf-nicely result "")))

(defun print-lf-nicely (lfs prefix)
  (when lfs
    (format t "~%~A" prefix)
    (format t "~S" (car lfs))
    (mapcar #'(lambda (x) (print-lf-nicely x (concatenate 'string prefix "  ")))
	    (cdr lfs)))
  (values))

(defun extract-lfs (term)
  "is a term has an LF then it is returned rather than the term"
  (when term
    (if (consp term) 
      (if (eq (car term) 'term)
	  (or (add-positional-info (find-arg-in-act term :lf) term) term)
	  (mapcar #'extract-lfs term))
      term)))

(defun add-positional-info (lf term)
  (if (or *no-positions-in-lf* (find-arg-in-act lf :start))
      lf
      (append lf
	      (list :start (find-arg-in-act term :start) :end (find-arg-in-act term :end)))))

(defun sort-lfs (root lfs)
  "Sorts LFs by closet distance to the 'root' LF, which is the first one"
  (multiple-value-bind (root-lfs other-lfs)
      (split-list  #'(lambda (y) (eq (second y) root)) lfs)
    (let* ((root-lf (car root-lfs))   ;; there should be only one
	   (id (second root-lf)))
      (when (and root-lf (not (member id *id-seen*)))
	(setq *id-seen* (cons id  *id-seen*))
	(let* ((next-ones (remove-if #'(lambda (z) (member z *id-seen*))
				  (let ((type (third root-lf)))
				    (append (get-every-other-value (cdddr root-lf))
					    ;; value may be in complex types as well
					    (if (and (consp type) (eq (car type) 'ont::set-of))
						(list (second type)))))))

					#||	     (consp (second type)) (eq (car (second type)) 'ONT::LAMBDA))
						(get-every-other-value (cdddr (second type))))))))||#
	       (rest
		(mapcar #'(lambda (x)
		      "we're just putting the LF with the id X to the front"
		      (sort-lfs x other-lfs))
			next-ones)))
	  (cons root-lf rest)))
      )))

(defun find-lf (id lfs)
  (find-if #'(lambda (y) (eq (second y) id)) lfs))

(defun get-every-other-value (x)
  "Makes a list of the values from a keyord arg list"
  (when x
    (let ((val (cadr x)))
      (cond
       ((symbolp val)
	  (cons val (get-every-other-value (cddr x))))
       ((and (consp val) (every #'symbolp val))
	(append val (get-every-other-value (cddr x))))
       (t (get-every-other-value (cddr x))))
      )))
		      

(defun print-tree (tree &optional (stream nil))
  (pprint-logical-block 
    (stream nil)
     (let* ((cat (remove-packages (car tree)))
            (args (remove-packages (cdr tree)))
            (feats (find-arg args :feats))
            (stats (find-arg args :stats))
            (id (find-arg stats :id))
            (subs (find-arg args :sub))
	    (prob (find-arg stats :prob))
            (score (find-arg stats :score))
            (rule-id (find-arg stats :rule))
            (pos (find-arg stats :pos)))
       (if id (format stream "~S: " id))
       (format stream "~S" (cons '% (cons cat feats)) )
       (if pos (format stream " from ~S to ~S" (car pos) (second pos)))
       (if rule-id (format stream " using rule ~S" rule-id))
       (if prob (format stream " [p=~,3F]" prob))
       (if score (format stream " [score=~,3F]" score))
       (mapcar #'(lambda (sub)
                   (pprint-newline :mandatory stream)
                   (format stream "  ")
                   (print-tree sub stream)
                   )
               subs))))


;;-----------------------
;;
;; swift -- added print-kr-types
;; 
;; display structured kr type and slots for an lf
;;

(defun KR-transform (lfs)
  "this just removes the additional arguments"
  (let ((x (om::transform lfs)))
    x))

(defun get-lf-term (lfs id)
  "return the lf term associated with this identifier"
  (remove-if #'null (mapcar #'(lambda (lf-term)
				(if (eq (second lf-term) id)
				    lf-term))
			    lfs))
  )

(defun get-roles-for-lf-term (roles var)
    (when roles
      (append
       (case (car roles)
	 (:TMA nil)
	 (:MODS nil)         
         (otherwise (list (list (car roles) var (cadr roles)))))
       (get-roles-for-lf-term (cddr roles) var))))

(defun get-kr-term (krs id)
  "return the kr term associated with this identifier"
  (remove-if #'null (mapcar #'(lambda (kr-term)
				(member id kr-term))
			    krs))
  )

(defun get-kr-slot (kr-term)
  "return the kr slot from this term"
  (third (cadar kr-term))
  )

(defun get-kr-type-and-slots (kr-lambda)
  "return the kr triple from this term"
  (cdr (cdadar kr-lambda))
  )

(defun print-kr-for-single-utt (parse)
  "given an lf, return the KR action and slots with KR type for the main lf (typically the term associated with :content)
"
    (when parse
      (let* ((lfs (extract-lfs (find-arg-in-act parse :terms)))
	     (utt-content-id (find-arg-in-act (car lfs) :content))
	     (content-lf (get-lf-term lfs utt-content-id))
	     (roles (get-roles-for-lf-term (cdddar content-lf) (cadar content-lf)))
	     (krs (kr-transform lfs))
	     (content-term (get-kr-term (car krs) utt-content-id))
	     (content-triple (get-kr-type-and-slots content-term))
	     (instance (third (first content-triple)))
	     (slots (cdr content-triple))
	     )
	(reverse (append
		  (mapcar #'(lambda (role)
			      (car (remove-if #'null (mapcar #'(lambda (slot)
								 (let ((role-id (third role)))
								   (when (find role-id slot)
								     (list (first slot)
									   (third (get-kr-slot (get-kr-term (car krs) role-id)))
									   ))))
								    slots))))
				 roles) (list instance))))
    )
  )

(defun print-kr-types (&rest args)
  (let* ((parse (eval (list* 'get-parse :lf args))))
    
    (if (eq (car parse) 'compound-communication-act)
	(progn (format t "~%COMPOUND ACT:")
	       (mapcar #'print-kr-for-single-utt (find-arg-in-act parse :acts)))
      (print-kr-for-single-utt parse)
      )))


;;------------------------
;; get-pview-tree
;;
;; Put the best parse into a flat list.
;;
(defun get-pview-tree ()
  (list 'parse-tree
        (flatten (get-answers '(w::LEX 1 2 3 4 5 6 7 8 9))) ))

;;;;; defined in UTIL
;;;#||(defun flatten (lis)
;;;  (if lis
;;;      (if (listp (car lis))
;;;          (append (flatten (car lis)) (flatten (cdr lis)))
;;;        (cons (car lis) (flatten (cdr lis))) )
;;;    nil ))
;;;||#

;;------------------------
;; show-constit
;;
;; printing the parse tree rooted at the indicated constituent
;;
(defun show-constit (C &optional features)
  (let ((entry (get-entry-by-name C)))
    (if (entry-p entry)
      (if (consp features)
        (print-abbrev-tree (util::convert-to-package features *w-package*) 0 entry nil)
        (print-abbrev-tree nil 0 entry nil))
      (parser-warn "~S is not a defined constituent in the chart" C)))
  (values))

;;---------------------------
;;
;; Upward trace
;;

(defun show-parents (c &key (max-levels 2) (features '(w::lex)))
  "Show all parents of a constituent with a given name, going up at most max-levels, and displaying the features specified"
  (when (>= max-levels 0) 
    (let ((entry (get-entry-by-name C)))
      (if (entry-p entry)
	  (let ((parents (find-all-parents c)))	    
	      (print-blanks max-levels)
	      (show-entry-with-name entry features)
	      (pprint-logical-block (nil nil)
		(dolist (parent parents)
		  ;;	      (format t "--------------------Level ~S--------------- ~%" max-levels)
		  (show-parents (entry-name parent) :max-levels (- max-levels 1) :features features)
		  ;;		  (pprint-newline :mandatory)
		  ))
	    )
	(parser-warn "~S is not a defined constituent in the chart" C)))
    )
  (values))

(defun find-all-parents (c)
  "Given a constituent, finds  all entries in the chart which have it as a child"
  (remove-if-not #'(lambda (x) (is-child c x))
		 (get-constits-from-chart nil nil))
  )		 
  
			
(defun is-child (c entry)
  "True if c is a child of an entry"
  (find-if #'(lambda (x)	
       (and (numberp (first x)) (eql (second x) c)))
	   (constit-feats (entry-constit entry)))
  )

;;  UTILITY FUNCTIONS

(defun get-input-sequence (constit-name &optional need-brackets)
  "finds the word sequence that produce the constituent"
  (let ((entry (get-entry-by-name constit-name)))
    (if (entry-p entry)
        (get-input-seq-from-entry entry need-brackets)
      (parser-warn "~S is not a defined constituent in the chart" constit-name))))


(defun get-input-seq-from-entry (entry &optional need-brackets)
  "Actually find the words with guarantee that constit is OK"
  (let* ((constit (entry-constit entry))
         (answer
          (or
           ;; first check if we already know the input
           (let ((word (get-value constit 'w::input)))
                 (if (and word (equal word '(NIL)))
                   (if (consp word)
                     (copy-list word)
                     (list word))))
           ;; otherwise, find it
           (let*
             ((subconstitnames (getsubconstitnames 1 constit))
              (subentries (mapcar #'get-entry-by-name
                                  subconstitnames)))
             (if subentries
               (mapcan #'(lambda (e)
                           (get-input-seq-from-entry e need-brackets))
                       subentries)
               (let ((word (get-value constit 'w::lex)))
                 (if (consp word)
                   (copy-list word)
                   (list word))
                 )
               )
             ))))
       answer))

(defun get-input-seq-from-constit (constit &optional need-brackets)
  "Same as get-input-from-entry, but starting from a constit"
  (let* ((subconstitnames (getsubconstitnames 1 constit))
         (subentries (mapcar #'get-entry-by-name
                              subconstitnames)))
    (if subentries
        (mapcan #'(lambda (e)
                    (get-input-seq-from-entry e need-brackets))
                subentries)
      (let ((input-val (get-value constit 'w::input)))
	(when (not (listp input-val)) ;; a non-nil symbol or other value
	  (setq input-val (list input-val)))
	(or input-val (list (get-value constit 'w::lex)))
	;;      (list (or (get-value constit 'w::input) (get-value constit 'w::lex))))))
	)
      )
    ))

(defun gather-names (start end)
  (cond ((= start end)
         (aref (get-constits-by-position) start))
        ((< start end)
         (append (aref (get-constits-by-position) start)
                 (gather-names (+ 1 start) end)))
        (t nil)))

(defun show-entry-with-name (entry &optional features)
  (pprint-logical-block (nil nil)
    (format t "~s: " (remove-packages (entry-name entry)))
    (pprint-logical-block (nil nil)
      (format-constit (entry-constit entry) features)
      (write-char #\space)
      (format t "from ~S to ~S using rule ~S" (entry-start entry) 
              (entry-end entry) (entry-rule-id entry))
      (format t " [P = ~,4F S = ~,4F]" (entry-prob entry) (calculate-entry-score entry))
      (format t "~%"))))

(defun format-constit (constit features &optional (stream nil))
  "Prints out a constituent"
   (when (constit-p constit)
       (let ((feats (remove-packages
		     (remove-if #'(lambda (x)
                                   (null (cadr x)))
                               ;; remove unwanted features (if indicated)
                               (if (consp features)
                                 (remove-if-not #'(lambda (fv)
                                                    (member (car fv) features))
                                                (constit-feats constit))
                                 (constit-feats constit))))))
         (pprint-logical-block (stream feats :prefix "(% " :suffix ")")
           (write (remove-packages (constit-cat constit)) :stream stream)
           (write-char #\space stream) 
	   (when (constit-optional constit) 
	     (write '(w::OPTIONAL +) :stream stream)
	     (write-char #\space stream) 
	     )
	   (loop
             (format-term (pprint-pop) features stream)
             (pprint-exit-if-list-exhausted)
             (write-char #\space stream ) 
             (pprint-newline :fill stream))
	  ))))


(defun format-term (term features &optional (stream nil))
  (cond
   ((var-p term)
    (format-var term features stream))
   ((constit-p term)
    (format-constit term features stream))
   ((consp term)
    (pprint-logical-block (stream term :prefix "(" :suffix ")")
      (format-term (car term) ;;(remove-packages (car term))
		   features stream)
      (mapcar #'(lambda (x)
                  (write-char #\space stream)
                  (format-term x features stream))
            (remove-packages (cdr term)))))
   (t
    (format stream "~S" term))))

(defun format-var (term features &optional (stream nil))
  (pprint-logical-block (stream nil)
    (write-string "?" stream)
    (when (var-non-empty term)
      (write-string "!" stream))
    (write (remove-packages (var-name term)) :stream stream)
    (when (var-values term)
      (format-var-values (var-values term) features stream))))

(defun format-var-values (values features &optional (stream nil))
  (pprint-logical-block (stream nil :prefix ":(" :suffix ")")
    (cond 
     ;; A hack! always assumes arrays are sems. 
     ((and (arrayp values) (not (stringp values)))
      (let* ((vallist (build-list-from-sem-array values));;(remove-packages (build-list-from-sem-array values)))
	     )
	;; Now we go to write features
	(pprint-logical-block (stream (cdr vallist))
	  (format-term (car vallist) features stream)
	  (write-char #\Space stream) 
	  (loop	    
	    (format-term (pprint-pop) features stream)
	    (pprint-exit-if-list-exhausted)
	    (write-char #\Space stream) 
	    (pprint-newline :fill stream))
	  (pprint-newline :fill stream))
	))
     ((listp values)
      (pprint-logical-block (stream values)
	(loop	    
	  (format-term (pprint-pop) features stream)
	  (pprint-exit-if-list-exhausted)
	  (write-char #\Space stream) 
	  (pprint-newline :fill stream))))
     ((stringp values)
      )
     (t
      (format-term values features stream))))
  )

;;------------------------
;; get-entry-with-name
;;
;; like (show-entry-with-name), but no printing.
;;
(defun get-entry-with-name (entry)
  "This builds an entry without the RHS for various output formats"
  (MAKE-ENTRY :NAME (entry-name entry)
              :CONSTIT (entry-constit entry)
              :START (entry-start entry)
              :END (entry-end entry)
              :RULE-ID (entry-rule-id entry)
	      :prob (entry-prob entry)
              ))

;;------------------------
;; print-abbrev-tree
;;
;; Prints out an abbreviated constituent, indicating just the category,
;;   subconsituents and the lexical items
;;
(defun print-abbrev-tree (features prefix entry bindings)
  ;;(format t "~%Printing ~S with bindings" (entry-name entry) bindings)
  (let ((tree (get-tree entry bindings)))
    (pat (convert-to-package features :w) prefix tree)))

(defun pat (features prefix tree)
  (when tree
    (print-blanks prefix)
    (show-entry-with-name (car tree) features)
    (mapcar #'(lambda (e) (pat features (1+ prefix) e)) (cdr tree))))

(defun print-blanks (n)
 (dotimes (i n)
   (format t "  ")))

;;------------------------
;;  BUILD-CONSTIT-TREE
;;  Given an entry and set of bindings, this builds
;;  a parse tree rooted at the entry, inserting the input feature as we go along

(defun build-constit-tree (entry bindings)
  (let ((tree (get-tree entry bindings)))
    (BCT tree bindings)))

(defun BCT (tree bndgs)
  (when tree
    (cons (insert-positional-info 
	   (insert-input-feature (entry-constit (car tree))) (car tree))
        (mapcar #'(lambda (e)
                    (BCT e bndgs))
		(cdr tree)))))
              
(defun insert-input-feature (constit)
  "for each object, we find the input string that generated and add it as a feature"
  (if (consp (get-value constit 'w::input))
      constit
      (replace-feature-value constit 'w::input (get-input-seq-from-constit constit))))

(defvar *no-positions-in-lf* t)

(defun insert-positional-info (constit e)
  (let ((lf (get-value constit 'w::LF)))
    (if (constit-p lf)
	(replace-feature-value constit 'w::LF (insert-posn-in-lf lf (entry-start e) (entry-end e)))
	constit)))
			      

(defun insert-posn-in-lf (lf start end)
  (add-feature-value 
   (add-feature-value (insert-positions-in-pros lf start end)
		      'w::start start)
   'w::end end))
							
(defun insert-positions-in-pros (lf start end)
  "checks for any embedded *PRO* forms and inserts the start and end"
  (let ((constraints (get-value lf 'w::constraint))
	(newlf (copy-constit lf)))
    (cond ((constit-p constraints)
	  (replace-feature-value newlf 'w::constraint 
			       (make-constit :cat '&
					     :feats (mapreuse #'(lambda (fv)
								(insert-pos-in-feat fv start end))
							      (constit-feats constraints)))))
	  (t newlf))))

(defun mapreuse (fn ll)
  (when ll
    (reuse-cons (apply fn (list (car ll)))
		(mapreuse fn (cdr ll))
		ll)))

(defun insert-pos-in-feat (fv start end)
  (let ((val (cadr fv)))
    (cond ((and (constit-p val) (eq (constit-cat val) 'w::*PRO*))
	   (list (car fv) (insert-posn-in-lf val start end)))
	  ((and (consp val) (every #'(lambda (x) (and (constit-p x) (eq (constit-cat x) 'w::*PRO*))) val))
	   (list (car fv)
		 (mapcar #'(lambda (x) (insert-posn-in-lf x start end)) val)))
	  (t fv))))


     
;;------------------------
;; print-list
;;
;; Print given list row by row.
;;
(defun print-list (s lis indent)
  "Prints arbitrary lists, one element per row, indented."
  (dolist (item lis)
    (progn (dotimes (i indent) (princ " " s))
           (format s "~S~%" item) ))
    (values) )

(defun worddef (w &optional feats id)
  (setq w (convert-to-syntax-package w))
  (setq feats (convert-to-syntax-package feats))
  (setq id (convert-to-syntax-package id))  
  (if (null id)
    (mapcar #'(lambda (x)
                (cond
                 ((lex-entry-p x)
                  (format t "~&~S:~%    " (lex-entry-id x))
                  (format-constit (lex-entry-constit x) (or feats (get-trace-features)))
                  (format t " [p=~S]~%~%" (lex-entry-prob x)))
                 ((and (consp x) (eq (car x) 'composite) (lex-entry-p (third x)))
                  (let ((c (third x)))
                    (format *standard-output* "~%~S: As the start of sequence ~S~% " 
                            (lex-entry-id c) (cons w (second x)))
                    (format-constit (lex-entry-constit c)
                                    (or feats (get-trace-features)))
                    (format t " [p=~S]~%~%" (lex-entry-prob c)))
                  )
                 (t (parser-warn "Bad lexicon entry: ~S" x))))
           
	     (remove-if #'(lambda (e)
			    (and (lex-entry-p e)
				 (eq (constit-cat (lex-entry-constit e)) 'w::word)))
			(lib-retrieve-from-lex w nil))
	     )
           
    ;; look up a particular entry by ID
    (let ((entry (find-if #'(lambda (x)
                              (eq (lex-entry-id x) id))
                          (gethash w (get-lexicon)))))
      (if (lex-entry-p entry)
          (format-constit (lex-entry-constit entry) (get-trace-features)))))
  (values))

(defun extract-slots (c)
  (append (list (constit-cat c))
          (get-value-if-there c 'w::LF)
          (get-value-if-there c 'w::SEM)
          (get-value-if-there c 'w::SUBJ)
          (get-value-if-there c 'w::DOBJ)
          (get-value-if-there c 'w::COMP3)
          (get-value-if-there c 'w::IOBJ)
          (get-value-if-there c 'w::part)))

(defun get-value-if-there (c f)
  (let ((v (get-value c f)))
    (if (and c (not (eq c '-)) (not (eq c *empty-constit*)))
        (list (list f (if (constit-p v) (get-value v 'w::sem) v))))))

;;=====================================
;;  BUILD-NEW-LF
;;
;;  Here is the code to convert the old output to the new logical form.
;;  Eventually we should rewrite this to extract this form directly and eliminate the
;;  old getanalysis code

(defvar *additional-modifiers* nil)

(defun add-additional-modifier (m)
  (setq *additional-modifiers* (cons m
                                     *additional-modifiers*)))

(defun build-new-lf (analysis form)
  (if (eq (car analysis) 'compound-communications-act)
    (list 'compound-communication-act
           :acts (mapcar #'(lambda (x) (build-new-lf x form))
                         (find-arg-in-act analysis :acts))
	   :words (find-arg-in-act analysis :words)
	   :start (get-min-position)
	   :end (get-max-position))
    (build-new-lf-for-speech-act analysis form)))

;; This is where we'd convert the LF MODS into more specific roles
(defun build-new-lf-for-speech-act (analysis form)
  " Additional terms that are generated from *PRO* constits during the interpretation
    are colected in the *additional-modifiers* and appended onto the result at the end"
  (setq *additional-modifiers* nil)
  (let 
    ((objects (find-arg-in-act analysis :objects))
     (channel (find-arg-in-act analysis :channel))
     (uttnum (find-arg-in-act analysis :uttnum))
     (root (convert-to-package (find-arg-in-act analysis :root) *ont-package*))
     (words (find-arg-in-act analysis :words))
     )
    (let*
      ((terms
        (append 
         (remove-if #'null
                    (mapcar #'(lambda (x)
                                (build-lf-term (cdr x) form))
                            objects))
         *additional-modifiers*))
       (root-term (find-if #'(lambda (x) (eq (find-arg-in-act x :var) root)) terms))
       (result (list 'UTT
                     :type (car analysis)
		     :channel channel
                     :root root
                     :terms (refine-abstract-roles-in-terms terms)
		     :uttnum uttnum
		     :start (or (find-arg-in-act root-term :start) (get-min-position))
		     :end  (or (find-arg-in-act root-term :end) (get-max-position))
		     :words words
                     ))
       (noise (find-arg-in-act analysis :noise))
       (focus (find-arg-in-act analysis :focus)))
      ;; add additional information on noise, adverbials and focus
      (if noise 
        (setq result (append result (list :noise noise))))
      (if focus
        (setq result (append result (list :wh-focus focus))))
      result
      )))

(defun sort-terms (root terms)
  "Puts the terms into top-down tree structure"
  (let* ((*id-seen* nil)
	 (sorted-lfs (mapcar #'cadr (flatten-tree (sort-lfs root (mapcar #'(lambda (x) (find-arg-in-act x :lf)) terms))))))
    (mapcar #'(lambda (x) (find-term X terms)) sorted-lfs)))
	 
(defun find-term (id terms)
  (find-if #'(lambda (x) (eq (find-arg-in-act x :var) id)) terms))

(defun flatten-tree (tree)
  (when tree
    (cons (car tree)
	  (mapcan #'flatten-tree (cdr tree)))))


(defun build-lf-term (c form)
  (when (and (consp c) (eql (car c) '%))
    (let ((term (case (second c)
		  (w::description
		   (build-lf-term-from-description (clean-out-vars c) form))
		  ((w::prop w::speechact W::sa-seq)
		   (build-lf-term-from-prop (clean-out-vars c) (second c) form)))))
      (if *kr-type-info-desired* 
	  ;; promote the WNSENSE into the LF
	  (list* 'term :lf (car (extract-lfs+wnsense (list term) *kr-type-info-desired* t)) (cdddr term))
	  term))))

(defun build-lf-term-from-description (c form)
  ;;
  ;;  Transforming terms
  ;;
  ;;  Descriptions are a consituent of CAT DESCRIPTION with
  ;;       STATUS - indicates the type of term: definite, indefinite, pro, wh, universal, how-much
  ;;       TYPE - the LF type of the object (either simple, ot more complex %PRED types.
  ;;       SEM - the SEM of of the object
  ;;       CONSTRAINT - the constraints on the object, as a single constraint as a list, or an constituent of CAT AND.
  
  ;;    converts to (<specifier> <var> <constraint>)
  (trace-msg 3 "~%~%Starting conversion of term ~S" c)
  (let* ((desc (third c))
         (status (get-fvalue desc 'w::status))
	 (class (get-fvalue desc 'w::class))
	 (sem (get-fvalue desc 'w::sem))
         (input (get-fvalue desc 'w::input)) 
         (lex (get-fvalue desc 'w::lex))
	 (var (convert-to-package (get-fvalue desc 'w::var) *ont-package*))
	 (start (get-fvalue desc 'W::start))
	 (end (get-fvalue desc 'w::end))
	 (constraint-list (get-fvalue desc 'w::constraint))
	 )
    (if (eq constraint-list '-) (setq constraint-list nil))
    (cond
     ((member status '(ONT::name ONT::gname))
      (let ((name (if (and lex (not (eq lex '-)))
		      (if (consp lex) lex (list lex)) input)))
	(if name (setq constraint-list (insert-into-& (list :name-of name) constraint-list)))
	))
     ((eq status 'ONT::number)
      (let ((val (get-fvalue desc 'w::val)))
	(if val (setq constraint-list (insert-into-& (list :number val) constraint-list)))
	))
     ((eq status 'ONT::pro)
      ;; add a proform feature for the pro unless it is already there
      (setq constraint-list (if (not (get-fvalue (third constraint-list) 'w::proform))
;				(insert-into-& (list :proform (or input (list lex))) constraint-list)
				(insert-into-& (list :proform (or input lex)) constraint-list)
				constraint-list))))
    (let* ((newlf (list* (build-spec status)
			 var
			 (build-type-for-lf (or class sem))
			 (build-roles-for-lf var constraint-list)))
	   ;;  Build the expanded LF form useful for reference
           (term (list 'TERM :LF newlf)))
      (if (or (null form) (member 'w::var form))(setq term (append term (list :VAR var))))
      (if (or (null form) (member 'w::sem form)) (setq term (append term (list :SEM sem))))
      (if (and input (or (null form) (member 'w::input form)))
	  (setq term (append term (list :INPUT input)))
        )
      (setq term (append term (list :start start :end end)))
      term)))

(defun insert-into-& (v x)
  "adds a constraint into an & list"
  (cond
   ((null x) `(% & (,v)))
   ((and (consp x) (eq (car x) '%) (eq (cadr x) '&))
    (cons '%
	  (cons '&
		(list (cons v (third x))))))))

#||(defun clean-out-vars (expr)
  (cond
   ((isvar expr)
    (let* ((vnam (second expr))
	   (val (third expr))
	   (negated (fourth expr));;(eql (char (symbol-name vname) 0) #\!))
	   )
      (if (listp val)
	  (let ((reduced-val (remove-if #'null val)))
	    (if (eql (list-length reduced-val) 0)
		nil
		(if (eql (list-length reduced-val) 1)
		    (car val)
		    #||(if (null (car val))    ;; this is commented out as it generates stuff that not a valif LF form at times
			nil
			(if negated
			    (list 'ONT::NOT (car reduced-val))
			    (car val)))||#
		    (if (eq (car val) '$)
			(mapcar #'clean-out-vars reduced-val)
			(car val)))))
			#||(if negated
			    (list 'ONT::NOT (cons 'ont::or reduced-val))
			    (cons 'ONT::or reduced-val))))))||#
	  val)))
   ((consp expr)
    (mapcar #'clean-out-vars expr))
   (t expr)))||#

(defun clean-out-vars (expr)
  (cond
   ((isvar expr)
    (let* ((vnam (second expr))
       (val (third expr))
       (negated (fourth expr));;(eql (char (symbol-name vname) 0) #\!))
       )
      (if (listp val)
      (let ((reduced-val (remove-if #'null val)))
        (if (eql (list-length reduced-val) 0)
        nil
        (if (eql (list-length reduced-val) 1)
            (car reduced-val)
	    (if (eq (car reduced-val) '$)
            (mapcar #'clean-out-vars reduced-val)
            (car val)))))
      val)))
   ((consp expr)
    (if (eq (car expr) 'w::sem)
	(if (consp (cadr expr))
	    (list 'w::sem (third (cadr expr)))
	    expr)  ; when we have (sem -)
	(mapcar #'clean-out-vars expr))
    )
   (t expr)))

(defun build-spec (status)
  "Build the right SPEC structure based on the status feature"
  (case status 
    ((w::definite w::name w::gname) 'ont::the) 
    (w::indefinite 'ont::A)
    ((w::indefinite-plural w::number) 'ont::indef-set)
    (w::SM 'ont::SM)
    (w::definite-plural 'ont::the-set)
    ((w::wh w::what w::which w::whose w::*wh-term*) 'ont::wh-term) 
    (w::wh-quantity 'ont::wh-quantity)
    (w::universal 'ont::all-the)
    (w::value 'ont::value)
    (w::pro 'ont::pro)
    (w::pro-set 'ont::pro-set)
    (w::*pro* 'ont::impro)
    (w::direct 'ont::the)
    (w::kind 'ont::kind)
    (w::some-amount-of 'ont::some-amount-of)
    (w::bare 'ont::bare)
    (w::quantity-term 'ont::quantity-term)
    (w::quantifier 'ont::quantifier)
    ((w::F w::prop) 'ONT::F)
    (w::speechact 'ONT::Speechact)
    (w::sa-seq 'ONT::sa-seq)
    ;;; repeat the above but replace W:: by ONT::
    ((ONT::definite ONT::name ONT::gname) 'ont::the) 
    (ONT::indefinite 'ont::A)
    ((ONT::indefinite-plural ONT::number) 'ont::indef-set)
    (ONT::SM 'ont::SM)
    (ONT::definite-plural 'ont::the-set)
    ((ONT::wh ONT::what ONT::which ONT::whose ONT::*wh-term*) 'ont::wh-term) 
    (ONT::wh-quantity 'ont::wh-quantity)
    (ONT::universal 'ont::all-the)
    (ONT::value 'ont::value)
    (ONT::pro 'ont::pro)
    (ONT::pro-set 'ont::pro-set)
    (ONT::*pro* 'ont::impro)
    (ONT::direct 'ont::the)
    (ONT::kind 'ont::kind)
    (ONT::some-amount-of 'ont::some-amount-of)
    (ONT::bare 'ont::bare)
    (ONT::quantity-term 'ont::quantity-term)
    (ONT::quantifier 'ont::quantifier)
    ((ONT::F ONT::prop) 'ONT::F)
    (ONT::speechact 'ONT::Speechact)
    (ONT::sa-seq 'ONT::sa-seq)
    ;;;
    (otherwise (parser-warn "~%building LF term: unknown status type ~S" status) 'unknown)
    ))

(defun build-type-for-lf (class)
  (convert-var (convert-class class)))

(defun convert-class (class)
  "Converts CLASS feature to the new TYPE format"
   (cond
    ((symbolp class) class)
    ((Or (And (consp class) (eq (second class) 'w::pred))
         (and (constit-p class) (eq (constit-cat class) 'w::pred)))
     (break "archiac code encoutered"))
     ;;`(ont::set-of ,(build-set-type-for-lf class)))
    ((consp class)
     (case (car class)
       #||(% (if (eq (second class) 'w::pred)
	       (let* ((constraints (third class))
		      (v (get-fvalue constraints 'w::arg)))
		 (list 'ont::set-of 
		       (list* 'ONT::LAMBDA v (get-fvalue constraints 'w::class)
			      (build-roles-for-lf v (get-fvalue constraints 'w::constraints)))))
	     class))
       (w::set-of  `(ont::set-of ,(convert-to-package (second class) *ont-package*)))||#
       (?)
       ($ (simplify-class (ontologymanager::convert-sem-to-lf-type (map-or-back-to-? class)))) 
       (otherwise class)))))

(defun simplify-class (class)
  (if (and (consp class) (equal (car class) 'ont::<OR>) 
	   (eq (list-length (cdr class)) 1))
      (cadr class)
      class))

(defun map-or-back-to-? (sem)
  "This changes the format back to the ? form so the conversion works"
  (cond ((symbolp sem) sem)
	((consp sem)
	 (if (eq (car sem) 'f::OR)
	     (list* '? 'x (cdr sem))
	   (cons (map-or-back-to-? (car sem))
		 (map-or-back-to-? (cdr sem)))))))

(defun convert-var (class)
  "If class is a var, we map to a disjunction"
  (if (consp class)
      (case (car class)
	(? (if (third class) (cons 'w::OR (third class)) 'ONT::ANY-SEM))
	(f::OR
	 (if (eq (length class) 2) (second class) class))
	(otherwise class))
    class))

(defun build-set-type-for-lf (set-type)
  "Builds a set type - either a simple pred or a lambda term"
  (let*  ((feats (if (Constit-p set-type) (constit-feats set-type) (third set-type)))
          (class (convert-to-package (build-type-for-lf (get-fvalue feats 'w::class)) *ont-package*))
          (var (or (get-fvalue feats 'w::arg) (gen-symbol 'X)))
          (constraints (get-fvalue feats 'w::constraint)))
    (if (or (null constraints) (eq constraints '-))
      class
      (list* 'ONT::LAMBDA 
             var
             class
             (build-roles-for-lf var constraints)))))

(defvar *lf-roles-to-suppress* '(w::LSUBJ w::LOBJ w::LIOBJ w::LCOMP -))

(defun build-roles-for-lf (lf-var c)
  "Building the role restrictions"
  (when (not (eq c '-))
    (if (consp c)
	(let ((answer
            (cond 
              ((null (first c)) nil)
              ((and (eq (car c) '%) (eq (second c) '&))
               ;;(multiple-value-bind (times locs path others)
                                    ;;(split-out-time-loc-paths (third c))
                 ;;(append
                  (mapcan #'(lambda (x) (build-role-for-lf lf-var x))
                          (remove-if #'(lambda (x) (member (car x) *lf-roles-to-suppress*)) (third c))))
                  ;;(build-reified-role lf-var :AT-TIME 'ONT::TIME-LOC times)
                  ;;(build-reified-role lf-var :AT-LOC 'ONT::LOCATION locs)
                  ;;(build-reified-role lf-var :PATH 'ONT::PATH path))))
	      ((or (null (second c)) (eq (second c) '-))   ;; just ignore roles with empty values
		nil)
	      ((and (symbolp (first c)) (second c))  ;; atomic role name and non null value
	       (cons (build-role-for-lf lf-var c)
		       (build-roles-for-lf lf-var (cdr c)))
	       )
	      (t (parser-warn "Bad constraint list expression found: ~S Is it a list? ~S" c (consp c))
		 c))))
	  (collapse-multiple-features (remove-duplicate-roles answer) :MODS))
      (when c
        (parser-warn "Bad constraint list expression found: ~S" c)
        c))))
    
(defun remove-duplicate-roles (roles)
  "removes any identical instances of feature-value pairs"
  (when roles
    (let* ((r (car roles))
	  (v (cadr roles))
	  (rest (cddr roles))
	  (possible-dup (find-arg rest r)))
      (if (eq v possible-dup)
	  (remove-duplicate-roles (cddr roles))
	  (list* r v (remove-duplicate-roles (cddr roles)))))))

(defun collapse-multiple-features (ll feat)
  "this function finds all instances of the specified feature and
   builds a new feature with a list of the values. Used for :MODS and :ASSOC-WITH"
  (multiple-value-bind
          (c-s others)
          (extract-mods ll feat)
          (if (eq (list-length c-s) 0)
            others
            (append others (list feat c-s)))))

(defun extract-mods (ll feat)
  (if ll
    (multiple-value-bind
      (c-s others)
      (extract-mods (cddr ll) feat)
      (if (eq (car ll) feat)
        (values (cons (second ll) c-s) others)
        (values c-s (cons (car ll) (cons (second ll) others)))))))

(defun build-reified-role (lf-var rname type constraints)
  (when constraints
    (let* ((reif-var (gen-symbol 'R))
          (mods (mapcar #'(lambda (c)
                              (build-modifier reif-var (list (car c) (subst reif-var lf-var (second c))))
                              )
                        constraints)))
      (list rname
            (build-reified-object type reif-var (list :MODS mods))
                ))))

(defun find-obj-defn (obj-id obj-list)
  (car (remove-if-not #'(lambda (x) (eq (second x) obj-id)) obj-list)))

(defun build-reified-object (type var roles)
  (add-additional-modifier
   (list 'TERM :VAR var :LF (append (list 'w::A var type) roles))
   )
  var)
  

(defun build-role-for-lf (var c)
  "converts roles into keyword format, and builds a :MODS constraint for other modifiers"
    (if (or (null c) (and (consp c) (eq (second c) '-)))
      (list)
       (cond
       ((symbolp (car c))
        (if (eq (car c) '%)
          ;; an embedded prop, will be on the term list elsewhere
	    (list :MODS (get-fvalue (second c) 'w::var))
          ;;  otherwise, just build role
          (if (second c)
	      ;; convert the semantic scale feature to the ontology package for AKRL
	      ;;(if (eq (car c) 'w::scale)
	      ;;(list (keywordify (car c)) (build-value (util::convert-to-package (second c) :ont)))
	      (let ((val (second c)))
		(list (keywordify (car c))
		      (convert-to-ont-if-in-parser-package val))))))
       (t (consp (car c))  
          (list :MODS (build-modifier var c))
       ))))

(defun convert-to-ont-if-in-parser-package (val)
  (cond ((symbolp val)
	 (if (eq (symbol-package val) *parser-package*)
	     (build-value (util::convert-to-package val *ont-package*))
	   val))
	((numberp val)
	   val)
	((consp val) (mapcar #'convert-to-ont-if-in-parser-package 
			     val))
	(t val)))
       

(defun build-modifier (lf-var mod)
  "Builds a new term for the modifier and saves it on *additional-modifiers* for insertion in LF at end of processing"
    (let ((var (gen-symbol *symbol-prefix*)))
      (add-additional-modifier
       (list 'term :VAR var
             :LF (if (consp mod)
                   (if (eq (second mod) lf-var)
                           (list 'ONT::F var (car mod) :THEME lf-var)
                           (list 'ONT::F var (car mod) :OF lf-var :IS (build-value (second mod)))
                                 )
                   (list 'UNKNOWN_MOD :value mod))
             ))
      var))

(defun build-value (v)
  "Build a value, cleaning up variables etc"
  (cond 
   ((symbolp v) v)
   ((numberp v) v)
   ((consp v) 
    (case (car v) 
      (?
       (let ((value (cddr v)))
         (case (list-length value)
           (1 (if (null (car value))
                'unbound-var
                (car value)))
           (0 'UNKNOWN)
           (otherwise (cons 'w::OR value)))))
      (:* v)
      (otherwise  v)))))  

(defvar *promote-tmas* nil)              

(defun build-lf-term-from-prop (c type form)
  "Builds a set of terms from a PROP constituent"
  (let* ((args (third c))
         (class (get-fvalue args 'w::class))
         (var (convert-to-package (get-fvalue args 'w::var) *ont-package*))
         (constraints (get-fvalue args 'w::constraint))
         (tma (get-fvalue args 'w::tma))
         (roles (build-roles-for-lf var constraints))
         (input (get-fvalue args 'w::input))
	 (start (get-fvalue args 'W::start))
	 (end (get-fvalue args 'w::end))
         (new-lf (list* (build-spec type)
             var
             class
             (if (and tma (not (eq tma '-)) (not (symbolp tma)))
               (if (not *promote-tmas*)
		   (append  roles `(:TMA ,(third tma)))
		   (append roles (build-roles-for-lf var tma)))
	       roles)))
         (term (list 'TERM :LF new-lf)))
    (if (or (null form) (member 'w::var form)) (setq term (append term (list :VAR var))))
    (if (or (null form) (member 'w::sem form)) (setq term (append term (list :SEM (get-fvalue args 'w::sem) ))))
    (if (or (null form) (and (member 'w::input form) input))
      (setq term (append term (list :INPUT input)))
        )
    (if (not *no-positions-in-lf*)
	(setq term (append term (list :start start :end end))))
    term))
  
(defun remove-packages (x)
  "for functions that print to terminal, we remove package prefixes unless pflag not NIL"
  x)
  #||(if *print-package-flag* x
    (util::convert-to-package x :parser)))||#

;; optional code for simplifying numeric expressions

(defun simplify-numbers (utt)
  (if (and (consp utt) (eq (car utt) 'compound-communication-act))
      (replace-arg-in-act utt :acts (mapcar #'simplify-numbers (find-arg-in-act utt :acts)))
    (let* ((root (find-arg-in-act utt :root))
	   (terms (find-arg-in-act utt :terms))
	   (lfs (mapcar #'(lambda (x) (find-arg-in-act x :lf)) terms)))
    (multiple-value-bind
	(numerics other)
	(split-list #'(lambda (x) (and (eq (car x) 'ONT::quantity-term)
				       (or (eq (third x) 'ONT::NUMBER)
					   (match-with-subtyping (third x) 'ONT::NUMBER-UNIT))))
		    lfs)
      (if  numerics
	  (multiple-value-bind
	      (numbers units)
	      (split-list #'(lambda (x) (eq (third x) 'ONT::NUMBER)) numerics)
	    (let ((newnumbers 
		   (recursively-simplify-units units numbers)))
	      (replace-arg-in-act utt :terms
				  (sort-terms root (install-back-into-terms terms (append (reverse other) newnumbers))))))
	utt)))))

(defvar *change-made* nil)

(defun recursively-simplify-units (units numbers)
  "recursively handle the simple cases until there are no more"
  (setq *change-made* nil)
   (let ((new-lfs
	  (reduce-sums (transform-unit-expressions units numbers))))
     (if *change-made*
	 ;; we made a change so try again
	 (multiple-value-bind
	     (numbers units)
	     (split-list #'(lambda (x) (eq (third x) 'ONT::NUMBER)) new-lfs)
	   (recursively-simplify-units units numbers))
       new-lfs
     )))

(defun transform-unit-expressions (lfs numbers)
  "Simplifies each of the LFs if possible given the current simplified numbers"
  ;;(format t "~%Transforming ~S with ~S" lfs numbers)
  (if (and lfs numbers)
    (let* ((lf (car lfs))
	   (quan (find-arg-in-act lf :quan))
	   (unit (third lf))
	   (val-expression (find-if #'(lambda (x) (eq (second x) quan)) numbers))
	   (digit (find-arg-in-act val-expression :value))
	   (val (compute-value unit digit))
	   (new-lf (if val  (list 'quantity-term (second lf) 'ONT::NUMBER :value val)))
	   (remaining-nums (if val
			       (remove-if #'(lambda (x)
					      (eq (second x) (second val-expression)))
					  numbers)
			     numbers))
	 )
   
    (if new-lf (setq *change-made* t))
    (if (cdr lfs)
	(multiple-value-bind
	    (new remaining-numbers)
	    (transform-unit-expressions (cdr lfs) remaining-nums)
	  (append (cons (or new-lf lf) new)
		  remaining-numbers))
      (append (if new-lf (list new-lf) (list lf)) remaining-nums)))
    (append lfs numbers)))
      
(defun compute-value (unit val)
  (if (consp unit)
      (setq unit (third unit)))
  (when (and unit (numberp val))
    (case unit
    (W::hundred (* val 100))
    (W::thousand (* val 1000))
    (W::dozen (* val 12))
    (W::million (* val 1000000))
    )))

(defun reduce-sums (lfs)
  (multiple-value-bind
      (sums numbers)
      (split-list #'(lambda (x) (find-arg-in-act x :sum)) lfs)
    (if (null sums)
	numbers
      (reduce-each-sum sums numbers))))

(defun reduce-each-sum (sums numbers)
  (if (null sums)
      numbers
    (let* ((sum (car sums))
	   (vars (find-arg-in-act sum :sum))
	   (firstlf (find-if #'(lambda (x) (eq (second x) (car vars))) numbers))
	   (secondlf (find-if #'(lambda (x) (eq (second x) (second vars))) numbers))
	   (firstval (find-arg-in-act firstlf :value))
	   (secondval (find-arg-in-act secondlf :value))
	   )
      (if (and (numberp firstval) (numberp secondval))
	  (progn
	    (setq *change-made* t)
	    (cons (list 'quantity-term (second sum) 'ONT::NUMBER :value (+ firstval secondval))
		  (reduce-each-sum (cdr sums)
				   (remove-if #'(lambda (x) (member (second x) (list (second firstlf) (second secondlf))))
					      numbers))))
	(cons sum
	      (reduce-each-sum (cdr sums) numbers))))
    ))
		      
   
(defun install-back-into-terms (terms lfs)
  "puts the lfs back into the terms"
  (mapcar #'(lambda (x)
	      (replace-lf-in-term x terms))
	      lfs))

(defun replace-lf-in-term (lf terms)
  (let* ((var (second lf))
	(term (find-if #'(lambda (x) (eq (find-arg-in-act x :var) var)) terms)))
    (replace-arg-in-act term :lf lf)))

;;------------------------
;; extract-fragments
;;
;; extract "meaningful" fragments from a cca for display
;;
(defun extract-fragments (lf)
  (let ((actlist (cadr (member :acts (cdr lf))))
	(return-list nil)
	)
    (dolist (act actlist)
      (let ((sa (second (member :type act)))
	    (lf-list nil)
	    (words nil)
	    (initial t)
	    (not-frag t))
	;; for now the fragment fragments aren't interesting
	(when (and (not (eql sa 'w::SA_FRAGMENT ))
		   (not (eql sa 'w::SA_PRED-FRAGMENT ))
		   )
	  ;; collect all the lfs for the "interesting" fragments	  
	  (dolist (term (cadr (member :terms (cdr act))))
	    (let ((lf (second (member :lf term))))
	      (if (eql (third lf) 'w::SA_PRED-FRAGMENT)
		  (setq not-frag nil))
	      (when initial (setq words (second (member :input term)))
		    (setq initial nil)
		    )
	      ;; add full lfs for non-pred fragments
	      (when not-frag 
		(push (second (member :lf term)) lf-list)
		)
	      )
	    )
	  (push (list sa words (reverse lf-list)) return-list)
	  )
	)
      )
    (reverse return-list))
  )

(defvar *parser-package* (find-package "PARSER"))

(defun show-tree (&optional feats)
  (show-t1 (list (car (get-answers))) feats))

(defun show-t1 (tree feats)
  (let ((features (if (or (null feats) (null (car feats)))
		      '(W::lex)
		      feats)))
    (format t "~%CAlling show-t1 with feats ~S and features ~S" feats features)
    (when tree
      (mapcar #'(lambda (x)
		  (cons (convert-to-package (car x)  *parser-package*)
			(let ((sub (find-arg-in-act x :SUB))
			      (feat-list (find-arg-in-act x :FEATS)))
			  (if sub 
			      (show-t1 sub features)
			      (cons :feats (convert-to-package 
					    (mapcar #'(lambda (f)
							(list f (get-fvalue feat-list f))) features) *parser-package*))))
			))
	      tree))))

;;  Here's new code for converting to the new LF format from the actual data structures
;;    rather than the list form. Eventually should be integrated into the parser and
;;    old stuff removed


(defun build-lf-from-constit (old-lf)
  (case (constit-cat old-lf)
      (w::description
       (build-lf-from-description (constit-feats old-lf)))
      ((w::prop w::speechact W::sa-seq)
       (build-lf-from-prop (constit-feats old-lf) (constit-cat old-lf)))
      ))

(defun build-lf-from-description (desc)
  ;;
  ;;  Transforming LF from constit form to reduced list form
  ;;
  ;;  Descriptions are a consituent of CAT DESCRIPTION with
  ;;       STATUS - indicates the type of term: definite, indefinite, pro, wh, universal, how-much
  ;;       TYPE - the LF type of the object (either simple, ot more complex %PRED types.
  ;;       SEM - the SEM of of the object
  ;;       CONSTRAINT - the constraints on the object, as a single constraint as a list, or an constituent of CAT AND.
  
  ;;    converts to (<specifier> <var> <constraint>)
  (trace-msg 3 "~%~%Starting conversion of term ~S" desc)
  (let* ((status (get-fvalue desc 'w::status))
	 (class (get-fvalue desc 'w::class))
	 (sem (get-fvalue desc 'w::sem))
	 (lex (get-fvalue desc 'w::lex))
	 (var (get-fvalue desc 'w::var))
	 (input  (get-fvalue desc 'w::input))
	 (constraint-list (get-fvalue desc 'w::constraint))
	 )

    (if (eq constraint-list '-) (setq constraint-list nil))
    (cond
     ((member status '(ONT::name ONT::gname))
      (let ((name (or (if lex (if (consp lex) lex (list lex)) input))))
	(if name (setq constraint-list (insert-into-& (list :name-of name) constraint-list)))
	))     
     ((eq status 'ONT::number)
      (let ((val (get-fvalue desc 'w::val)))
	(if val (setq constraint-list (insert-into-& (list :number val) constraint-list)))
	))
     ((eq status 'ONT::pro)
      ;; add a proform feature for the pro unless it is already there
      (setq constraint-list (if (not (get-fvalue (third constraint-list) 'w::proform))
;				(insert-into-& (list :proform (or input (list lex))) constraint-list)
				(insert-into-& (list :proform (or input lex)) constraint-list)
			      constraint-list))))
    (list* (build-spec status)
	   var
	   (build-type-for-lf (or class sem))
	   (build-roles-from-lf-constit var constraint-list))))
	   
(defun build-lf-from-prop (args type)
  "Builds a set of terms from a PROP constituent"
  (let* ((class (get-fvalue args 'w::class))
         (var (get-fvalue args 'w::var))
         (constraints (get-fvalue args 'w::constraint))
         (tma (get-fvalue args 'w::tma))
         (roles (build-roles-from-lf-constit var constraints))
	 (new-lf (list* (build-spec type)
             var
             class
             (if (and tma (not (eq tma '-)) (not (symbolp tma)))
               (append  roles `(:TMA ,(build-roles-from-lf-constit nil tma)))
	       roles)))
         )
    new-lf))
  

(defun build-roles-from-lf-constit (lf-var c)
  "Building the role restrictions"
  (when (and c (not (eq c '-)))
    (if (constit-p c)
	;;(multiple-value-bind (times locs path others)
	   ;; (split-out-time-loc-paths (constit-feats c))
	  ;;(collapse-multiple-features
	   ;;(append
	    (mapcan #'(lambda (x) (build-role-for-lf lf-var x))
		    (remove-if #'(lambda (x) (member (car x) '(w::LSUBJ w::LOBJ w::LIOBJ w::LCOMP -))) (constit-feats c)))
	    ;;(build-reified-role lf-var :AT-TIME 'ONT::TIME-LOC times)
	    ;;(build-reified-role lf-var :AT-LOC 'ONT::LOCATION locs)
	    ;;(build-reified-role lf-var :PATH 'ONT::PATH path))
	   ;;:mods))
	(when c
	  (parser-warn "Bad constraint list expression found: ~S" c)
	  (list c)))))

(defun refine-abstract-roles-in-terms (terms)
  (let* ((lfs (mapcar #'(lambda (x) (find-arg-in-act x :LF)) terms))
	 (type-map (mapcar  #'(lambda (x) (list (second x) (third x))) lfs)))
    (mapcar #'(lambda (term)
		(replace-arg-in-act term :lf (refine-abstract-roles-in-lf (find-arg-in-act term :lf) type-map))) terms)))

(defun refine-abstract-roles-in-lf (lf type-maps)
  (let ((roles (cdddr lf)))
  (list* (car lf) (second lf) (third lf)
	 (refine-abstract-roles roles type-maps (get-type (third lf)) roles))))


;;+++++++++++++++
;;  This is the key function for refining the semantic roles by replacing the
;;   abstract mods roles generated by the adverbials
(defun refine-abstract-roles (roles type-maps lf-type existing-roles)
  (when roles
    (case (car roles) 
      ((:mod :result)
	(let* 
	    ((arg (cadr roles))
	     (argtype (get-type (cadr (assoc arg type-maps)))))
	  (list* (refine-role lf-type argtype (car roles) existing-roles)
		 arg
		 (refine-abstract-roles (cddr roles) type-maps lf-type existing-roles))))
      (:mods 
       (refine-abstract-roles (append (expand-mods (cadr roles)) (cddr roles)) type-maps lf-type existing-roles))
      (otherwise
       (list* (car roles) (second roles) 
	      (refine-abstract-roles (cddr roles) type-maps lf-type existing-roles))))))

(defun expand-mods (mods)
  " given a list of mods vars, explodes them into a list with :mod feature"
  (mapcan #'(lambda (x) (list :mod x)) mods))

;; This is the mapping table of format: ((Predicate-type  ((arg-type*) newrole)*)*)
(defvar *role-mapping-table* nil)


(defun refine-role (lf-type arg-type old-role-name existing-roles)
  (return-first-refinement
   (mapcar #'(lambda (x) (map-role x lf-type arg-type old-role-name)) *role-mapping-table*)
   old-role-name
   existing-roles))
	 

(defun map-role (map-info lf-type arg-type old-role-name)
  (if (and (om::subtype lf-type (caar map-info))
	   (eq (cadar map-info) old-role-name)
	   )
      (let ((new-role-names (find-if #'(lambda (x) 
				     (if (consp (car x))
					 (find-if #'(lambda (xx)
							  (om::subtype arg-type xx)) (car x))
					 (om::subtype arg-type (car x))))
				     (cdr map-info))))
	(or (cadr new-role-names) old-role-name))
      old-role-name))

(defun return-first-refinement (new-names old-role-name existing-roles)
  (let ((really-new-names (remove-if #'(lambda (x)
				  (eq x old-role-name)) new-names)))
    (cond 
      ;; first case checks for relational roles that we can have multiple versions of
      ((member (car really-new-names)
		   '(:result :source :transient-result))
	   (car really-new-names))
      ;; otherwise we look for first one that doesn't already exist
      ((member (car really-new-names) existing-roles)
       (return-first-refinement (cdr really-new-names) old-role-name existing-roles))
      ;; otherwise, the first one is the answer
      (really-new-names 
       (car really-new-names))
      ;; none left, retrn original name
      (t old-role-name))))

      					    
						
(setq *role-mapping-table*
      '(
	((ont::situation-root :mod)
	 ((ont::at-loc) :location))  ;; at-loc is always location
	((ont::send :mod)
	 ((ont::to-loc ont::goal-reln  ont::direction-reln) :RESULT)
	 ((ont::position-reln) :location))
	((ont::motion :mod)
	 ((ont::pos-as-containment-reln) :location)
	 ((ont::to-loc ont::position-reln ont::goal-reln ont::direction-reln) :result)
	 ((ont::source-reln) :source))	
	((ont::motion :result)
	 ((ont::obj-in-path ont::trajectory) :transient-result)
	 )
	((ont::put :mod)
	 ((ont::to-loc ont::position-reln ont::goal-reln  ont::direction-reln) :result)
	 ((ont::source-reln) :source)
	 )
	((ont::apply-force :mod)
	 ((ont::to-loc ont::position-reln ont::goal-reln  ont::direction-reln) :result)
	 ((ont::source-reln) :source))	
	((ont::apply-force :result)
	 ((ont::obj-in-path ont::trajectory) :transient-result)
	 )
	((ont::giving :mod)
	 ((ont::to-loc) :result)
	 ((ont::source-reln) :source)
	 )
	((ont::acquire :mod)
	  ((ont::to-loc ont::position-reln ont::goal-reln  ont::direction-reln) :result)
	 ((ont::source-reln) :source))
	((ont::acquire :result)
	 ((ont::obj-in-path ont::trajectory) :transient-result)
	 )
	((ont::joining :mod)
	  ((ont::goal-reln) :result)  ; into
	 ((ont::source-reln) :source)
	 )
	((ont::change :mod)
	  ((ont::to-loc ont::goal-reln ont::direction-reln) :result)
	  ((ont::source-reln) :source))
	((ont::phys-object :mod)
	 ((ont::position-reln ) :location))
	 ;;((ont::assoc-with) :assoc-with))
	((ont::abstract-object :mod)
	 ((ont::position-reln) :location)
	;; ((ont::assoc-with) :assoc-with)
	 ((ont::degree-modifier) :degree)
	 )
	((ont::situation-root :mod)
	 ((ont::goal-reln) :result)
	 ((ont::reason ont::purpose) :reason)
	 ((ont::therefore) :result)
	 ((ont::extent-predicate) :extent)
	 ((ont::frequency ont::frequency-val) :frequency)
	 ((ont::degree-modifier) :degree)
	 ;;((ont::assoc-with) :assoc-with)
	 ((ont::temporal-location) :time)
	 ((ont::event-duration-modifier) :extent)
	 ((ont::instrument) :agent)
	 ((ont::evaluation-attribute-val) :evaluation)
	 ((ont::pos-condition) :condition)
	 ;;((ont::goal-reln) :goal)
	 ((ont::position-reln ) :location)
	 ((ont::accompaniment) :agent1)
	 ((ont::by-means-of ont::with-instrument) :method)
	 ((ont::beneficiary) :beneficiary)
	 ((ont::source-reln) :source)
	 ((ont::manner ont::abstract-object-property ont::pivot) :manner)
	 ((ont::likelihood ont::qualification) :qualification)
	 )
	)
 )

	 
(defun get-type (xx)
  (if (consp xx)
      (second xx)
      xx))

(defun lf (&optional n)
 ;; (print-parse :lf :xtra '(none) :nth (or n 1)))
  (if (or (null n) (numberp n))
      (progn
	(print-lfs :nth (or n 1))
	(values))
      (format t "~%Error: argument must be a number")))

(defun lf+ (&optional n)
  "same as (lf) but has an extra set of parentheses to make it more conveneient for cut and paste"
  (format t "(")
   (print-lfs :nth (or n 1))
   (format t ")")
   (values))

;; Here is code for promoting the KS senses into the LF

(defvar *kr-type-info-desired*)

(defun extract-LFs+wnsense (terms kr-info-desired normalize-wnsenses)
  (remove-if #'null
	     (mapcar #'(lambda (x)
			 (extract-LF+krinfo x kr-info-desired normalize-wnsenses))
		     terms)))

(defun extract-LF+krinfo (term kr-info-desired normalize-wnsenses)
  (let* ((sem-feats (util::find-arg-in-act term :sem))
	 (rawkrtype (when (listp sem-feats) (cadr (assoc 'F::KR-TYPE (cddr sem-feats)))))
	 (krtype  (when (and (consp rawkrtype) 
			     (every #'consp rawkrtype))
		    (mapcar #'(lambda (x)
				(cons (keywordify (car x)) (cdr x)))
						  rawkrtype))))
    (append (util::find-arg-in-act term :LF)
	    (list :start (util::find-arg-in-act term :start) 
		  :end (util::find-arg-in-act term :end))
	    (append-each-kr-info kr-info-desired krtype normalize-wnsenses))))

(defun append-each-kr-info  (kr-info-desired krtype normalize-wnsenses)
  (when kr-info-desired
    (if (eq (car kr-info-desired) :wnsense)
	(let* ((abnormal-wn-sense (cadr (convert-sense-list 
					 (list (car (get-domain-spec-info-from-krtype krtype :wn))))))
	       (wn-sense
		(if normalize-wnsenses
		    (if (consp abnormal-wn-sense)
			(mapcar #'normalize-wn-sense-triple
				abnormal-wn-sense)
			(normalize-wn-sense-triple abnormal-wn-sense))
		    abnormal-wn-sense)))
	  (append (if wn-sense (if (consp wn-sense) (list* :wnsense wn-sense)
				   (list :wnsense wn-sense)))
		  (append-each-kr-info (cdr kr-info-desired) krtype normalize-wnsenses)))
	;; all the others are handled simply
	(let ((info  (get-domain-spec-info-from-krtype krtype (car kr-info-desired))))
	  (append 
	   (if info (list (car kr-info-desired) info))
	   (append-each-kr-info (cdr kr-info-desired) krtype normalize-wnsenses))
	))
    ))
      
(defun get-domain-spec-info-from-krtype (krtype domain-name)
  "extract domain-name info  out of KRTYPE list"
  (remove-if-not #'(lambda (x) (eq (car x) domain-name)) 
		 krtype))  ;; changed as there my be multiple entries for a given domain-type
  ;;(cdr (assoc domain-name krtype)))
    

(defun convert-sense-list (wnsenses)
  (if (consp wnsenses)
      (cond ((eq (list-length wnsenses) 1)
	     (car wnsenses))
	    ((member (car wnsenses) '(ont::or ont::and))
	     wnsenses)
	    (t (list 'ont::AND wnsenses)))
      wnsenses))

(defun  normalize-wn-sense-triple (wn-sense)
  (if (consp wn-sense)
      (normalize-wn-sense (car wn-sense))
      wn-sense))

(defun normalize-wn-sense (wn-sense)
  "Convert a sense key to the sense key for the lexicographically first word in
   its synset. This ensures that we always refer to a synset by the same name.
   Also we generalize instance sense keys to their types"
  (when wn-sense ; pass through nil
    (when (consp wn-sense)
      (format t "~%~%WARNING: trying to normalize a list: ~S" wn-sense)
      (setq wn-sense (car wn-sense)))
    (let ((ss (wf::get-synset-from-sense-key wf::wm wn-sense)))
      (when ss
	(convert-wn-instance-to-type
	 (first
	  (sort
	   (wf::sense-keys-for-synset ss)
	    #'string<)))))))

(defun convert-wn-instance-to-type (wnsense)
  (let ((p (wf::get-pointers-by-relationship wf::wm
					     (wf::get-synset-from-sense-key wf::wm wnsense)
					     "@i")))
    (if (not p)
	;; not an instance
	wnsense
	;; map instance to type (actually first type listed - seen wmail from will 4/26/2013 if we want all
	(first (wf::sense-keys-for-synset (second (car p)))))))
