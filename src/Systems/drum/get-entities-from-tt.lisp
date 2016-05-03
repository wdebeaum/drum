(dolist (p '(:w :f :ont
	     :bto :chebi :co :go :pw :mi :hgnc :up :xfam :mesh :efo :uo :cvcl :fa))
  (make-package p))

(defun getk (l k)
  (second (member k l)))

(defun string-ends-with (str suffix)
  (and (<= (length suffix) (length str))
       (string= suffix (subseq str (- (length str) (length suffix))))))

(defun get-dsi-id (dsi)
  "Get an appropriate ID to use from a domain-specific-info list."
  (let ((drum (assoc 'drum dsi)) ; just take the first one and hope it's right
        (specialist (assoc 'specialist dsi))
	ns id)
    (cond
      (drum
        (let ((id-sym (getk drum :id)))
	  (setf ns (package-name (symbol-package id-sym))
	        id (symbol-name id-sym))))
      (specialist
        (setf ns 'specialist
	      id (symbol-name (getk specialist :eui))))
      )
    (when id
      (if (string= ns "COMMON-LISP")
	id
        (format nil "~a:~a" ns id)
	))))

(defun pfam-dsi-p (dsi)
  (and (eq 'drum (car dsi))
       (let* ((id (getk dsi :id))
              (id-name (symbol-name id)))
         (and (eq (find-package :xfam) (symbol-package id))
	      (<= 2 (length id-name))
	      (string= "PF" (subseq id-name 0 2))
	      ))
       ))

(defun subcell-dsi-p (dsi)
  (and (eq 'drum (car dsi))
       (let* ((id (getk dsi :id))
              (id-name (symbol-name id)))
         (and (eq (find-package :up) (symbol-package id))
	      (<= 2 (length id-name))
	      (string= "SL" (subseq id-name 0 2))
	      ))
       ))

(defun role-dsi-p (dsi)
  (and (eq 'drum (car dsi))
       (some (lambda (mapping)
               (member (second (getk mapping :through)) '(chebi::|50906| MI::|0500|)))
             (getk dsi :mappings))))

(defun ion-dsi-p (dsi)
  (and (eq 'drum (car dsi))
       (some (lambda (mapping)
               (eq (second (getk mapping :through)) 'chebi::|24870|))
             (getk dsi :mappings))))

(defun get-dsis-for-types (ont-types ont-type-to-dsis)
  "Given a list of ont-types ordered from most to least preferred, and an
   assoc-list from ont-types to lists of domain-specific-info structures, get
   the dsis associated with the first ont-type that has any at all."
  (loop for ot in ont-types
        for c = (assoc ot ont-type-to-dsis)
	when c do (return-from get-dsis-for-types (cdr c))))

(defun run (in out &key (first-passage-num 3))
  "Read messages from TextTagger from in and write a tab-separated table to out
   with these columns:
   passage number
   start offset
   end offset
   text
   entity type
   link to biological resource
   "
  ;; TODO?
  ;; - just get largest tag covering any given span
  ;; - remove duplicate terms
  (loop with passage-num = first-passage-num
        for msg = (read in nil nil)
  	while msg
	for content = (getk msg :content)
	do
    (case (car content)
      (word
        (destructuring-bind (_ text &key frame sense-info &allow-other-keys) content
	    (declare (ignore _))
	  (let (ont-type-to-dsis
	        all-ont-types
		all-dsis)
	    ;; reorganize ont-types and dsis so we can look at all of each at
	    ;; once, as well as the mapping between them
	    (dolist (si sense-info)
	      (destructuring-bind (&key trips-parts-of-speech ont-types domain-specific-info &allow-other-keys) si
		  (declare (ignore trips-parts-of-speech)) ; for now
		(loop for ot in ont-types
		      for c = (assoc ot ont-type-to-dsis)
		      do (cond
			   (c
			     (setf (cdr c)
			           (append (cdr c) domain-specific-info)))
			   (t
			     (push (cons ot domain-specific-info)
			           ont-type-to-dsis)
			     (push ot all-ont-types)
			     )
			   )
			 (setf all-dsis (append all-dsis domain-specific-info))
		      )))
	    (let* ((pfam-dsi (find-if #'pfam-dsi-p all-dsis))
		   (role-dsi (find-if #'role-dsi-p all-dsis))
		   (ion-dsi (find-if #'ion-dsi-p all-dsis))
		   (subcell-dsi (find-if #'subcell-dsi-p all-dsis))
		   (entity-type
		     (cond
		       ; this was preventing "AKT" because it was wrongly
		       ; tagged as a verb
;			 ((not (member 'w::n trips-parts-of-speech))
;			   nil)
		       (ion-dsi ; don't tag ions like Co2+
			 nil)
		       ((intersection all-ont-types '(ont::protein-family))
			 (when pfam-dsi
			   (setf all-dsis (list pfam-dsi)))
			 'PROTEIN_FAMILY)
		       ((intersection all-ont-types '(ont::protein ont::gene))
		         (setf all-dsis
			       (get-dsis-for-types '(ont::protein ont::gene)
						   ont-type-to-dsis))
		         (if (intersection all-ont-types
					   '(ont::event-of-change))
			   'PROTEIN_FAMILY
			   'GENE_PROTEIN))
		       ((intersection all-ont-types
				      '(ont::pharmacologic-substance))
		         (setf all-dsis
			       (cdr (assoc 'ont::pharmacologic-substance
					   ont-type-to-dsis)))
			 'DRUG)
		       ((and (not (member 'ont::cell-part all-ont-types))
			     (intersection all-ont-types
				           '(ont::chemical ont::molecule)))
			 (cond
			   (role-dsi
			     (setf all-dsis (list role-dsi))
			     'DRUG_CLASS)
			   ((member text '(" inhibitor" " inhibitors")
				    :test #'string-ends-with)
			     (setf all-dsis
			           (get-dsis-for-types
				       '(ont::chemical ont::molecule)
				       ont-type-to-dsis))
			     'DRUG_CLASS)
			   (t
			     (setf all-dsis
			           (get-dsis-for-types
				       '(ont::chemical ont::molecule)
				       ont-type-to-dsis))
			     'DRUG)
			   ))
		       ((member text '("cell" "cells") :test #'string-equal)
			 nil)
		       ((intersection all-ont-types '(ont::cell ont::cell-line))
		         (setf all-dsis
			       (get-dsis-for-types '(ont::cell-line ont::cell)
						   ont-type-to-dsis))
			 'CELL_LINE)
		       ((and subcell-dsi
			     (intersection all-ont-types '(ont::cell-part)))
		         (setf all-dsis (list subcell-dsi))
			 'SUBCELLULAR_LOCATION)
		       ((intersection all-ont-types '(ont::signaling-pathway))
		         (setf all-dsis (cdr (assoc 'ont::signaling-pathway
						    ont-type-to-dsis)))
			 'PATHWAY)
		       ((intersection all-ont-types
				      '(ont::macromolecular-complex))
		         (setf all-dsis (cdr (assoc 'ont::macromolecular-complex
						    ont-type-to-dsis)))
			 'COMPLEX)
		       )))
; DEBUG
;	      (format t "text = ~s~%entity-type = ~s~%role-dsi = ~s~%pfam-dsi = ~s~%ion-dsi = ~s~%all-dsis =~%~{  ~s~%~}ont-type-to-dsis =~%" text entity-type role-dsi pfam-dsi ion-dsi all-dsis)
;	      (dolist (c ont-type-to-dsis)
;	        (format t "~s~%~{  ~s~%~}" (car c) (cdr c)))
;	      (format t "~%")
	      (when entity-type
		(format out "~s~c~a~c~a~c~a~c~a~c~a~%"
			passage-num #\Tab
			(first frame) #\Tab
			(second frame) #\Tab
			text #\Tab
			entity-type #\Tab
			(get-dsi-id all-dsis)))
	      ))))
      (ok (incf passage-num))
      )))

