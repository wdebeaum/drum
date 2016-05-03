;; Unfortunately we can't read from the database like we do for ONT types, since the database doesn't keep all the information that's in the lisp files.

(defun getk (l k)
  (second (member k l)))

(defun geta (l a)
  (cdr (assoc a l)))

(defun geta1 (l a)
  (second (assoc a l)))

(defun xml-element-parts (e)
  "Given a lisp list representing an XML element, return three values:
   0. The element tag name
   1. The list of attributes (keyword args)
   2. The list of children
   "
  (loop with name = (car e)
        and attributes = nil
	for children = (cdr e) then (cddr children)
	while (keywordp (car children))
	do (setf attributes
	         (nconc attributes
		        (list (first children) (second children))))
	finally (return (values name attributes children))
	))

(defun merge-elements (a b &key (key #'identity) (test #'eql) (merge #'append))
  "Given two lists, a and b, merge the elements whose keys satisfy the test, by calling the given merge function and destructively replacing the element from a with the merged element. The returned list consists of the merged elements, plus the elements from each list that didn't match anything in the other.
  Note that two elements originally from b may also be merged; the first one will be added to a, and when the second one is reached it will match the first one in a."
  (dolist (b-element b)
    (let ((a-member (member (funcall key b-element) a :key key :test test)))
      (if a-member
        (setf (car a-member)
	      (funcall merge (car a-member) b-element))
	(push b-element a)
	)))
  a)

(defun merge-class-xmls (a b)
  "Given two lisp expressions representing the XML to be written for the same ONT type (XML CLASS element), return a new expression combining them."
  (multiple-value-bind (b-name b-attributes b-children)
      (xml-element-parts b)
      ;; assume words and ancestors are also the same, and frames are distinct
      (declare (ignore b-name b-attributes))
    (append a b-children)))

(defun merge-class-xml-lists (a b)
  "Given two lists of class XMLs, return a single merged list. May destructively modify a."
  (merge-elements a b :key (lambda (x) (getk x :onttype))
  		      :test #'equalp
                      :merge #'merge-class-xmls))

(defun merge-pos-xmls (a b)
  "Given two lisp expressions representing the XML to be written for the same POSm return a new expression combining them."
  (multiple-value-bind (a-name a-attributes a-children)
      (xml-element-parts a)
      (declare (ignore a-name)) ; always 'pos
    (multiple-value-bind (b-name b-attributes b-children)
        (xml-element-parts b)
	(declare (ignore b-name b-attributes)) ; should be the same as a
      `(pos ,@a-attributes
        ,@(merge-class-xml-lists nil (append a-children b-children))
	))))

(defun merge-pos-xml-lists (a b)
  "Given two lists of pos XMLs, return a single merged list. May destructively modify a."
  (merge-elements a b :key (lambda (x) (getk x :name))
  		      :test #'equalp
                      :merge #'merge-pos-xmls))

(defun merge-word-xmls (a b)
  "Given two lisp expressions representing the XML to be written for the same word, return a new expression combining them."
  (multiple-value-bind (a-name a-attributes a-children)
      (xml-element-parts a)
      (declare (ignore a-name)) ; always 'word
    (multiple-value-bind (b-name b-attributes b-children)
        (xml-element-parts b)
	(declare (ignore b-name b-attributes)) ; should be the same as a
      `(word ,@a-attributes
        ,@(merge-pos-xml-lists nil (append a-children b-children))
	))))

(defun merge-word-xml-lists (a b)
  "Given two lists of word XMLs, return a single merged list. May destructively modify a."
  (merge-elements a b :key (lambda (x) (getk x :name))
  		      :test #'equalp
                      :merge #'merge-word-xmls))

(defun comma-separated-list (l)
  (format nil "~{~a~^,~}" l))

(defun sorted-comma-separated-list (l)
  (comma-separated-list (sort (mapcar #'string l) #'string-lessp)))

(defun normalize-word-name (name)
  (string-downcase (if (consp name)
		     (format nil "~{~a~^_~}" (util::flatten name))
		     (format nil "~a" name))))

(defun space-separated-list-or-symbol (los)
  (if (consp los)
    (format nil "~{~a~^ ~}" (util::flatten los))
    (format nil "~a" los)))

(defun get-morphs-xmls (word pos)
  (mapcan
    (lambda (formcat)
      (let ((wfs (lxm::get-word-form word formcat)))
        (mapcar
	  (lambda (wf)
	    `(morph :cat ,(normalize-word-name formcat)
	            :to ,(normalize-word-name wf)
		    ))
	  wfs)))
    (case pos
      (W::V '(:12s123pbase :|3S| :past :pastpart :ing :nom)) ; FIXME W::be
      (W::N '(:sing :plur))
      (W::ADJ '(nil :er :est :ly))
      (otherwise nil)
      )
    ))

(defun get-examples-from-sense (sense)
  (format nil "~{~a~^; ~}"
	  (mapcan (lambda (arg)
		    (when (eq 'example (first arg))
		      (copy-list (cdr arg))))
		  sense)))

(defun sense-to-xml (sense default-templ)
  "Convert the given sense to an XML CLASS element."
  (let* ((onttype (or (geta1 sense 'lf-parent)
                      (geta1 sense 'lf)))
         (examples (get-examples-from-sense sense))
         (templ (or (geta sense 'templ) (list default-templ)))
	 (words (remove-duplicates (mapcar #'car (lxm::get-words-from-lf onttype)) :test #'equalp))
	 (ancestors (om::get-parents onttype))
	 )
    `(class :onttype ,(normalize-word-name onttype)
            :words ,(sorted-comma-separated-list (mapcar #'space-separated-list-or-symbol words))
	    :ancestors ,(comma-separated-list ancestors)
       (frame :desc ,(format nil "~{~a~^ ~}" templ)
              ,@(when (> (length examples) 0)
	          `(:example ,examples))))))

(defun define-words-to-xmls (dw)
  "Return the list of word XMLs defined in the given define-words expression."
  (let ((pos (getk dw :pos))
        (default-templ (getk dw :templ))
	(words (getk dw :words)))
    (mapcar
      (lambda (word-with-senses)
        (let ((word (car word-with-senses))
	      (senses (geta (cdr word-with-senses) 'senses)))
	  `(word :name ,(normalize-word-name word)
                            ;; unix time (1970 epoch)
	         :modified ,(- (get-universal-time) 2208988800)
	     (pos :name ,(normalize-word-name pos)
	       ,@(get-morphs-xmls word pos)
	       ,@(merge-class-xml-lists nil
	             (mapcar (lambda (sense)
	                       (sense-to-xml sense default-templ))
	                     senses))
	       ))))
      words)))

(defun define-list-of-words-to-xmls (dlow)
  "Return the list of word XMLs defined in the given define-list-of-words expression."
  (let* ((pos (getk dlow :pos))
         (default-templ (getk dlow :templ))
         (senses (getk dlow :senses))
	 (sense-xmls (mapcar (lambda (sense)
	                       (sense-to-xml sense default-templ))
	                     senses))
	 (words (getk dlow :words)))
    (mapcar (lambda (word)
              `(word :name ,(normalize-word-name word)
				;; unix time (1970 epoch)
		     :modified ,(- (get-universal-time) 2208988800)
	         (pos :name ,(normalize-word-name pos)
		   ,@(get-morphs-xmls word pos)
		   ,@sense-xmls)))
            words)))

(defun read-lexicon-data-file (filename)
  "Read the named lisp file containing define-words and define-list-of-words expressions, and return a list of expressions (one for each word) ready to be converted to XML."
  (with-open-file (f filename :direction :input)
    (let (word-xmls)
      (loop for def = (read f nil) while def do
        (setf word-xmls
	      (merge-word-xml-lists word-xmls
				    (ecase (car def)
				      (define-words
					(define-words-to-xmls def))
				      (define-list-of-words
				        (define-list-of-words-to-xmls def))
				      (in-package
				        nil)
				      ))))
      word-xmls)))

(defun sort-pos-and-classes (word-xml)
  "Make sure the POS tags are in order, and within them the CLASS tags are in order."
  (multiple-value-bind (name word-attributes pos-xmls)
      (xml-element-parts word-xml)
    `(,name ,@word-attributes 
      ,@(sort (mapcar (lambda (pos-xml)
                        (multiple-value-bind (name pos-attributes child-xmls)
			    (xml-element-parts pos-xml)
			  (multiple-value-bind (morph-xmls class-xmls)
			      (util::split-list
			          (lambda (x)
				    (eq 'morph (first x)))
			          child-xmls)
			    `(,name ,@pos-attributes
			      ,@morph-xmls
			      ,@(sort class-xmls
				      #'string-lessp
				      :key (lambda (x) (getk x :onttype))
				      )))))
                      pos-xmls)
              #'string-lessp
	      :key (lambda (x) (getk x :name))
	      ))))

(defun add-morph (word-xml pos morph)
  "Return the given word-xml with the given morph added under the given pos."
  (multiple-value-bind (name attributes pos-xmls)
      (xml-element-parts word-xml)
    `(,name ,@attributes
      ,@(if (member pos pos-xmls
                    :key (lambda (pos-xml)
		           (getk pos-xml :name))
		    :test #'string=
		    )
          ;; we already have the pos, add the morph to it
          (mapcar
	    (lambda (pos-xml)
	      (multiple-value-bind (name attributes child-xmls)
		  (xml-element-parts pos-xml)
		`(,name ,@attributes
		  ,@(when (string= pos (getk attributes :name))
		      (list morph))
		  ,@child-xmls)))
	    pos-xmls)
	  ;; else, add the pos around the morph
	  `((pos :name ,pos ,morph)
	    ,@pos-xmls)
	  ))))

(defun add-reverse-morphs (word-xmls)
  "Make the links between words and their morphed forms bidirectional, adding word xmls if necessary. Currently only does so for :nom links, since the others would be excessive."
  (let ((word-to-xml (make-hash-table :test #'equalp)))
    ;; make hash table from words to their xmls
    (dolist (word-xml word-xmls)
      (setf (gethash (getk word-xml :name) word-to-xml) word-xml))
    ;; add reverse morphs using the hash
    (dolist (word-xml word-xmls)
      (multiple-value-bind (name word-attributes pos-xmls)
          (xml-element-parts word-xml)
          (declare (ignore name))
	(let ((word (getk word-attributes :name)))
	  (dolist (pos-xml pos-xmls)
	    (multiple-value-bind (name attributes child-xmls)
		(xml-element-parts pos-xml)
		(declare (ignore name attributes))
	      (dolist (child-xml child-xmls)
		(multiple-value-bind (name attributes children)
		    (xml-element-parts child-xml)
		    (declare (ignore children))
		  (when (and (eq 'morph name)
			     (string= "nom" (getk attributes :cat)))
		    (let ((to (getk attributes :to)))
		      ;; make the word entry if it doesn't exist
		      (unless (gethash to word-to-xml)
			(setf (gethash to word-to-xml)
			      `(word :name ,to
						;; unix time (1970 epoch)
				     :modified ,(- (get-universal-time)
						   2208988800)
				     )))
		      ;; add the reverse nom morph
		      (setf (gethash to word-to-xml)
			    (add-morph (gethash to word-to-xml) "n"
				       `(morph :cat "nom" :from ,word)))
		      )))))))))
    ;; convert the hash values back to a list
    (loop for word-xml being each hash-value in word-to-xml collect word-xml)
    ))

(defun get-all-word-xmls ()
  (let (all-word-xmls)
    (dolist (filename (directory #!TRIPS"src;LexiconManager;Data;new;*.lisp"))
      (setf all-word-xmls
            (merge-word-xml-lists all-word-xmls
	                          (read-lexicon-data-file filename))))
    (mapcar #'sort-pos-and-classes (add-reverse-morphs all-word-xmls))))

(defun write-all-word-xmls (lexicon-data-dir)
  (dolist (word-xml (get-all-word-xmls))
    (with-open-file (f (format nil "~a/W\\:\\:~a.xml" lexicon-data-dir (getk word-xml :name)) :direction :output)
      (format f "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
      (format f "<?xml-stylesheet type=\"text/xsl\" href=\"../style/lookup.xsl\"?>~%")
      (format f "<!DOCTYPE WORD SYSTEM \"word.dtd\">~%")
      (format f (convert-lisp-to-xml word-xml))
      )))

