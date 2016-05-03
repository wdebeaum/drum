;; Nate Chambers
;; IHMC
;;
;; This file contains the functions that do syntactic conversion
;;

(in-package "ONTOLOGYMANAGER")


;; gf: 30 Oct 2009: This function is pretty gross. Previously it called a
;; function (current-scenario-name) which extracted the final element of
;; the scenario pathname and turned it into a symbol... every time it was
;; called! So now that I've made *scenario* a symbol, we cab just use it.
;; But why are we even calling this in cases where there is no
;; post-processing to do? And why is it specified here in the
;; ``generic'' LFMapper code, rather than in the `scenario'-specific directory
;; Data/LFTransforms/<scenario>? Unfortunately, going down that path turned up
;; so much crap that I am forced to just change the scenario test and leave
;; the rest for the future.
(defun transform-syntax (form)
  "@desc Converts the syntax of a lambda form into that which is more like
         the desired KR."
  ;; do a post-processing conversion if the domain calls for it
  (case trips:*scenario*
    (|epca| (lambda-to-km-syntax form))
    (|ralf| (lambda-to-ralf-syntax form))
    (t
     form)) ;; default condition is to do nothing
  )

(defun lambda-to-ralf-syntax (form)
  (setf form (create-military-time form))
  (handle-between-times form)
  )

(defun lambda-to-km-syntax (form)
  "@desc Code to manipulate the syntax of the lambda triples"
  form)

(defun handle-between-times (form)
  (cond ((null form) nil)
	;; lambda is a number-unit
	(t
	 (let* ((term (first form))
		(var (first term))
		(lam (second term))
		(between (is-pred-present 'ont::time-between lam)))
	   ;; if there is no unit or quantity in the number-unit, ignore
	   (when between
	     (setf lam (remove-pred 'ont::time-between lam))
	     (setf lam (append lam (list (list 'ont::after var (second (third between)))
					 (list 'ont::before var (first (third between)))))))
	   ;; recurse
	   (cons (list var lam) (handle-between-times (rest form)))))))

(defun to-military-time (hour ampm &key reverse)
  (cond (reverse ;; reverse the function
	 (if (> hour 12) (list (- hour 12) 'w::pm)
	   (list hour 'w::am)))
	((member ampm '(ont::pm w::pm (:* ont::time-object w::pm)) :test #'equal)
	 (+ 12 hour))
	(t hour)))

(defun create-military-time (form)
  (cond ((null form) nil)
	(t
	 (let* ((term (first form))
		(var (first term))
		(lam (second term))
		(hour (is-pred-present 'ONT::TIME-HOUR lam))
		(ampm (is-pred-present 'ONT::TIME-AMPM lam)))
	   (when (and hour ampm)
	     (when (and (eq (get-triple-value ampm) 'ont::pm)
			(< (get-triple-value hour) 12))
	       (setq hour (+ 12 (get-triple-value hour)))
	       ;; remove hour
	       (setf lam (remove-pred 'ont::time-hour lam))
	       ;; add the new time-hour
	       (setf lam (append lam (list (list 'ont::time-hour var hour)))))
	     ;; remove ampm
	     (setf lam (remove-pred 'ont::time-ampm lam)))
	   (cons (list var lam) (create-military-time (rest form))))
	 )))

(defun name-of-kr (nameof &key reverse)
  "@param nameof A list of lexical items in normal mode, a kr atom in reverse"
  (if (stringp nameof)
      ;; new handling: strings go through
      nameof
    ;; old handling, probably needs modification eventually
    (cond (reverse
	 ;; NOTE: should really split the hyphens up...
	 (list (util::convert-to-package nameof :w)))
	(t
	 (let ((str (hyphenate-list nameof)))
	   (if str (util::convert-to-package (read-from-string str) :ont)))
	 ))))

(defun lex-kr (lex &key reverse)
  "@param lex An atom in the :W package
   @returns An atom in the :ont package"
  (cond (reverse
	 (list (util::convert-to-package lex :w)))
	((atom lex)
	 (util::convert-to-package lex :ont))
	(t nil)))

(defun map-powerpc-name (name &key reverse)
  "@param name A list of lexical items (W::G 5)
   @desc Turns the list into a KM processor name 'PowerPC-G5'"
  (cond ((and reverse (stringp name)) ;; reverse it "PowerPC-G5"
	 (let ((num  (read-from-string (subseq name (1- (length name)))))
	       (type (read-from-string (subseq name 8 (1- (length name))))))
	   (list (util::convert-to-package type :W) num)
	   ))
	((and (listp name) (= 2 (length name)))
	 (format nil "PowerPC-~A~A" (car name) (second name))
	 )))

(defun mycar (list &key reverse)
  (if reverse (list list)
    (car list)))

(defun hyphenate-list (thelist)
  "@thelist a list of atoms
   @returns A single atom with the given atoms hyphenated"
  (cond ((null thelist) nil)
	((= 1 (length thelist)) (format nil "~A" (car thelist)))
	(t (format nil "~A-~A" (car thelist) (hyphenate-list (cdr thelist))))))

;; ----------------------------------------
;; KM Triples to KM Frame
;; ----------------------------------------
;; This code assumes the triples are of the form (<var> <predicate> <value>)

(defun km-triples-to-frame (triples &optional (head-id nil))
  (cond ((not (listp triples)) nil)
	(t
	 (let ((frames nil))
	   ;; extract all the frames (instance-of triples)
	   (dolist (trip triples)
	     (when (is-triple-type 'instance-of trip)
	       (setf frames (cons (list (get-triple-id trip)
					(get-triple-value trip)
					nil)
				  frames))))

	   ;; fill in the slots
	   (dolist (trip triples)
	     ;; if the triple is a slot (not an instance)
	     (when (not (is-triple-type 'instance-of trip))
	     (let* ((parent-id (get-triple-id trip))
		    (frame (assoc parent-id frames))
		    (slots (third frame)))
	       ;; add this triple to the list of slots
	       (setf slots (cons (list (get-triple-type trip)
				       (list (get-triple-value trip)))
				 slots))
	       ;; put the new list of slots into the frame
	       (setf (third (assoc parent-id frames)) slots))
	     ))
	
	   ;; insert syntax
	   (setf frames (mapcar #'put-frame-syntax frames))

	   ;; unify the variables with their frames
	   (setf frames (mapcar (lambda (x) (cons (car x)
						  (fill-frame frames (rest x))))
				frames))

	   ;; return the head frame, or all frames if no head was given
	   (if head-id (second (assoc head-id frames))
	     (mapcar #'second frames))
	   )
	 )))

(defun put-frame-syntax (frame)
  "@param frame (<var> <type> <list of slots>)
   @returns (a <type> with <list of slots>)"
  (list (first frame)
	(if (null (third frame)) ;; if no slots
	    (list 'a (second frame))
	  (append (list 'a (second frame) 'with)
		  (first (rest (rest frame)))))))

(defun fill-frame (frames frame)
  "@param frames A list of variable/frame pairs.  i.e. ((N3 (a Computer)) ...)
   @param frame A km frame.  i.e. (a Computer with ...)"
  (cond ((null frame) nil)
	((atom frame)
	 (let ((var (assoc frame frames)))
	   ;; if a variable
	   (if var (fill-frame frames (first (rest var)))
	     ;; else just return the atom
	     frame)))
	(t (cons (fill-frame frames (first frame))
		 (fill-frame frames (rest frame))))))

;; ----------------------------------------
;; KM case-sensitive lookup
;; ----------------------------------------
(defvar *km-atom-map* nil)

(defun akrl-to-km (akrl)
  (list* (first akrl)
	 (second akrl)
	 (mapcar #'(lambda (x) (atom-to-km x))
		 (cddr akrl))))

; same function name as OM/
;(defun triples-to-km (triples)
;  (mapcar #'triple-to-km triples))

(defun triple-to-km (triple)
  "@desc triple format: (id role value)"
  (list (first triple)
	(role-to-km (second triple))
	(value-to-km (third triple))))

(defun atom-to-km (atom)
  "Converts anything, roles and values"
  (if (keywordp atom) (role-to-km atom)
    (value-to-km atom)))

(defun role-to-km (role)
  "Converts roles (keywords or just the symbol names)"
  (base-to-km role))

(defun value-to-km (val)
  "@desc Converts the value into KM, with a special case handling
         of (:pair 1.2 *ghz) type constructs."
  (cond ((null val)
	 nil)
	((listp val)
	 (list :|pair| (second val) (base-to-km (third val))))
	((is-lf-variable val) val)	 
	((symbolp val)
	 (base-to-km val))
	(t val)))
      
(defun base-to-km (atom)
  "@param atom Must be an atom
   @desc Checks the hashmap to look for a KM equivalent of atom.
         If none is found, it lowercases the atom and puts pipes on it.
         **NOTE: package consistency is maintained with atom's package"
  (let* ((pkg (symbol-package atom))
	(name (intern (symbol-name atom) :om))
	(trans (or (gethash name *km-atom-map*)
		   (intern (string-downcase (string name))))))
    (intern (symbol-name trans) pkg)))
     

;; ----------------------------------------
;; Overhead functions
;; ----------------------------------------

(defun is-triple-type (type triple)
  "@desc Using the second element as the type"
  (and (listp triple) (eq type (second triple))))

(defun get-triple-id (triple)
  (if (and (listp triple) (= 3 (length triple)))
      (first triple) nil))

(defun get-triple-type (triple)
  (if (and (listp triple) (= 3 (length triple)))
      (second triple) nil))

(defun get-triple-value (triple)
  (if (and (listp triple) (= 3 (length triple)))
      (third triple) nil))

