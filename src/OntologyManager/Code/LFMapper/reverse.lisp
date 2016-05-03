;; Nate Chambers
;; IHMC
;;
;; Code to transform KR into LF
;;

(in-package "ONTOLOGYMANAGER")

;; NOTE: This function only takes the first value for each slot.  KM frame
;;       slots always have a list as its value.  I haven't seen an example
;;       where the list is more than one element, so for now I'm just taking
;;       the first value.  --NC
(defun km-frame-to-triples (km &optional frame-id)
  "@param km A valid km frame.  i.e. (a Computer with (...))
   @param frame-id An optional id to use for this frame in the triples.
   @returns A list of triples representing this frame."
  ;; if km is of the form (a X with ())
  (cond ((or (is-km-instance km) (is-km-definition km))
	 (if (null frame-id) (setf frame-id (unique-id)))
	   
	 (let ((triples nil))
	   ;; create the instance's triple
	   (if (is-km-definition km)
	       ;; i.e. (_Computer has (instance-of (Computer)) ..slots.. )
	       (setf triples (cons (make-triple frame-id 'instance-of
						(first (second (third km))))
				   triples))
	     ;; i.e. (a Computer with ..slots..)
	     (setf triples (cons (make-triple frame-id 'instance-of (second km))
				 triples)))
	   ;; if the frame has slots
	   (if (< 3 (length km))
	       (setf triples (append (create-slot-triples frame-id (subseq km 3))
				     triples)))
	   (reverse triples)))
	(t nil))
  )

(defun create-slot-triples (parent-id slots)
  "@desc Takes a list of slots from a KM frame (the list after 'with') and
         returns all the triples corresponding to these slots."
  (let ((triples nil))
    (dolist (slot slots)
      (setf triples (append triples (create-slot-triple parent-id slot))))
    (reverse triples)))

(defun create-slot-triple (parent-id slot)
  ;; make sure it is the proper format
  (cond ((is-km-slot slot)
	 (let ((triples nil) (slot-id nil))
	   ;; if the value is another frame
	   (cond ((is-km-instance (first (second slot)))
		  (setf slot-id (unique-id))
		  (setf triples (list (make-triple parent-id (first slot) slot-id)))
		  (setf triples (append triples
					(km-frame-to-triples (first (second slot))
							     slot-id))))
		 (t
		  (setf triples (list (make-triple parent-id (first slot)
						   (first (second slot))))))
		 )
	   triples))
	(t nil)))

;; change the ordering of the triple anyway you wish
(defun make-triple (id pred val)
  ;; listing the variable id first
  (list id pred val))

;; (a Computer with ...)
(defun is-km-instance (km)
  (and (listp km) (< 1 (length km)) (eq 'a (first km))))

;; (_Computer has (instance-of (Computer)) ...)
(defun is-km-definition (km)
  (and (listp km) (< 2 (length km)) (eq 'has (second km))))

(defun is-km-slot (slot)
  (and (listp slot) (= 2 (length slot)) (atom (first slot)) (listp (second slot))))
		   
  