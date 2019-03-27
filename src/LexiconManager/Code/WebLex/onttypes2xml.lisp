
(defvar *web* t "Are we generating files for the web (as opposed to a single hierarchical file)?")

(defun synset-sense-key (synset)
  "Get a sense key for the synset, using the first word in the synset."
  (wf::get-sense-key synset (slot-value (first (slot-value synset 'wf::words)) 'wf::word)))

(defun really-get-all-synsets (word)
  "Like wf::get-all-synsets, except it always uses morphy, and lists synsets in the order they would appear on the WordNet website."
  ;; FIXME actually, this gets more synsets than you would get by looking up
  ;; word on the website; e.g. you won't get "connected" if you look up
  ;; "connect" on the website, but you will get "connect" if you look up
  ;; "connected" :-P
  (let* ((poss (wf::get-parts-of-speech wf::wm))
         (synsets (mapcan (lambda (pos)
	                    (wf::get-synsets wf::wm word pos))
	                  poss))
         (forms (wf::run-morphy wf::wm word)))
    (dolist (form forms)
      (setf synsets (append synsets (wf::get-synsets-morpho wf::wm form))))
    (setf synsets
          (stable-sort (remove-duplicates synsets
	                                  :test #'wf::synsets-equal-p
					  :from-end t)
	               (lambda (ss1 ss2)
		         (< (position (wf::get-pos-string ss1) poss
			              :test #'string=
				      :from-end t)
			    (position (wf::get-pos-string ss2) poss
			              :test #'string=
				      :from-end t)
			    ))
		       ))
    synsets))

(defun synset-url (ss)
  "Return the URL for viewing the WordNet synset ss."
  (let* ((word (slot-value (first (slot-value ss 'wf::words)) 'wf::word))
         (word-synsets (really-get-all-synsets word))
	 (index (position ss word-synsets :test #'wf::synsets-equal-p)))
    (format nil "http://wordnetweb.princeton.edu/perl/webwn?s=~a&i=~s&o6=1"
	    (substitute #\+ #\_ word) index)))

;; takes a wordnet-synset structure
(defun wordnet-mapping-xml (ss)
  `(mapping
    :to "WordNet"
    :name ,(synset-sense-key ss)
    ,@(when *web*
      `(:url ,(synset-url ss)))
    ))

;; takes a sem-argument structure
(defun sem-argument-xml (arg)
  (with-slots (om::role om::optionality om::restriction) arg
    (with-slots (om::type om::features) om::restriction
      (if (null om::features)
	(list 'argument
	  :role om::role
	  :optionality om::optionality
	  :fltype (slot-value om::restriction 'om::type)
	  )
	(list 'argument
	  :role om::role
	  :optionality om::optionality
	  :fltype om::type
	  (features-xml om::features)
	  )
	)
      )
    )
  )

;; takes a feature-list structure
(defun feature-list-xml (fl)
  (with-slots (om::type om::features) fl
    (if (null om::features)
      (list 'sem
	:fltype om::type
	)
      (list 'sem
	:fltype om::type
	(features-xml om::features)
	)
      )
    )
  )

;; takes a list of features as found in the features slot of a feature-list structure
(defun features-xml (features)
  (append '(features)
    (mapcan
      (lambda (f)
	(read-from-string (format nil "(:~a ~a)" (first f) (second f)))
	)
      features
      )
    )
  )

;; takes an lf-description structure
(defun lf-description-xml (definition)
  (with-slots (om::name om::sem om::arguments om::parent om::children om::wordnet-sense-keys om::comment) definition
    (append
      `(onttype :name ,om::name)
      (when *web*
	`( :parent ,om::parent
	   ;; unix time (1970 epoch)
	   :modified ,(- (get-universal-time) 2208988800)
	  ,@(when om::comment `(:comment ,om::comment))
	  ,@(when om::sem
	      (list (feature-list-xml om::sem)))
	  ))
      (when *web* (mapcar #'sem-argument-xml om::arguments))
      (let ((words (sort (mapcar #'first (lxm::get-words-from-lf om::name)) (lambda (a b) (string< (format nil "~a" a) (format nil "~a" b))))))
        (mapcar (lambda (w) (list 'word :name (normalize-word-name w))) words)
	)
      (mapcar
          #'wordnet-mapping-xml
          (remove-duplicates
	      (remove nil
		  (mapcar
		      (lambda (sk)
		        (handler-case
			  (let ((ss (wf::get-synset-from-sense-key wf::wm sk)))
			    (unless ss
			      (format *error-output*
				      "No synset for sense key ~S~%" sk))
			    ss)
			  (error (e)
			    (format *error-output*
			        "failed to get synset for sense key ~S~%" sk))))
		      om::wordnet-sense-keys))
	      :test #'wf::synsets-equal-p))
      (let ((children
              (sort (copy-list om::children) 
                    (lambda (a b)
		      (string< (string a) (string b)))
		    )))
        (mapcar
	    (lambda (c)
	      (if *web*
	        (list 'child :name c)
		(lf-description-xml
		    (gethash c (om::ling-ontology-lf-table om::*lf-ontology*)))
		))
	    children))
      )
    )
  )

;; this function writes a file ONT::<type>.xml with the definition given
(defun write-ont-type-xml (ont-type definition lexicon-data-dir)
  (with-open-file (f
                   (if *web*
		     (format nil "~a/ONT\\:\\:~a.xml"
		             lexicon-data-dir
			     (string-downcase (string ont-type)))
		     (format nil "~a/TRIPS-ontology.xml" lexicon-data-dir)
		     )
		   :direction :output)
    (format f "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
    (format f "<!DOCTYPE ONTTYPE SYSTEM \"onttype.dtd\">~%")
    (format f (convert-lisp-to-xml (lf-description-xml definition)))
    )
  )

(defun write-all-ont-type-xmls (lexicon-data-dir)
  (let ((*web* t))
    (maphash (lambda (k v)
	       (write-ont-type-xml k v lexicon-data-dir))
	     (om::ling-ontology-lf-table om::*lf-ontology*))
    ))

(defun write-single-ontology-xml (lexicon-data-dir)
  (let ((*web* nil))
    (write-ont-type-xml 'ONT::root
        (gethash 'ONT::root (om::ling-ontology-lf-table om::*lf-ontology*))
	lexicon-data-dir)))

