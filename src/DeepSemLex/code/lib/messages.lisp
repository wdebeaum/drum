(in-package :deepsemlex)
(in-component :deepsemlex)

(defun echo-message (message args)
  (let ((message (get-keyword-arg args :text)))
    (format t message)))
(defcomponent-handler
    '(request &key :content (echo . *))
    #'echo-message
  :subscribe t)

(defun handle-load-dsl-file (message args)
  (progn 
    (load-dsl-file (get-keyword-arg args :filename))
    (send-msg '(tell :content (DSL-FILE-LOADED)))))
(defcomponent-handler
    '(request &key :content (load-dsl-file . *))
    #'handle-load-dsl-file
  :subscribe t)

(defun handle-get-sense (message args)
  (let ((word (get-keyword-arg args :word)))
    (print word)
    (send-msg `(tell :content (SENSE-RESPONSE :word ,word :sense ,(gethash word (senses *db*)))))))
(defcomponent-handler
    '(request &key :content (get-sense . *))
    #'handle-get-sense
  :subscribe t)

(defun handle-eval-path-expression (message args)
  (let* ((path (get-keyword-arg args :path))
	 (word (get-keyword-arg args :word))
	 (result (eval-path-expression path)))
    (print path)
    (print result)
    (send-msg `(tell :content (PATH-RESPONSE :word ,word :path-expression ,result)))))
(defcomponent-handler
    '(request &key :content (eval-path . *))
    #'handle-eval-path-expression
  :subscribe t)

(defun get-inherit-path-list (message args)
  (let* ((words (get-keyword-arg args :words))
	 (uttnum (get-keyword-arg args :uttnum))
	 (result-list (loop for word in words 
	 for result = (eval-path-expression (list word '(repeat 1 nil >inherit)))
	 collect (list word result))))
    (send-msg `(tell :content (PATH-LIST-RESPONSE :uttnum ,uttnum :paths ,result-list)))))
(defcomponent-handler
    '(request &key :content (eval-path-list . *))
    #'get-inherit-path-list
  :subscribe t)

(defun handle-get-concept (message args)
  (let ((concept (get-keyword-arg args :concept)))
    (print concept)
    (send-msg `(tell :content (CONCEPT-RESPONSE :concept-input ,concept :concept ,(gethash concept (concepts *db*)))))))
(defcomponent-handler
    '(request &key :content (get-concept . *))
    #'handle-get-concept
  :subscribe t)

(defun trips-concept-p (c)
  "Is c a concept from TRIPS (according to provenance name)?"
  (and (typep c 'concept)
       (some (lambda (p) (eq 'trips (name p))) (provenance c))))

(defun listify-concept-with-senses (c)
  "Like a simple listify, but also append the senses that directly inherit from
   c."
  (let* ((senses (eval-path-expression '(<inherit (when morph)) (list c)))
  	 (sense-lists (mapcar #'listify senses)))
    ;; remove links back up to c from sense-lists
    (setf sense-lists
          (mapcar (lambda (sense-list)
	            (remove-if
		      (lambda (i)
		        (and (listp i) (eq 'inherit (car i))))
		      sense-list))
	          sense-lists))
    `(concept ,(name c) ,@sense-lists)
    ))

(defun handle-get-trips-ont-mappings (message args)
  (let* ((concept-ids (get-keyword-arg args :concept-ids))
  	 (froms (eval-path-expression `(/ ,@concept-ids)))
	 mappings)
    (dolist (from froms)
      (let ((from-list (listify-concept-with-senses from))
            (throughs (eval-path-expression
	     '(
	       (* 
	          ;; unless we would get to a TRIPS concept
	          (unless >inherit #'trips-concept-p)
	          >inherit ; go up the inheritance hierarchy
		  )
	       ;; get only the concepts that actually mapped into trips
	       (when >inherit #'trips-concept-p)
	       )
	     (list from))))
        (dolist (through throughs)
	  (let ((through-list (listify-concept-with-senses through))
		(tos (eval-path-expression 
		  '(>inherit (when #'trips-concept-p))
		  (list through))))
	    (dolist (to tos)
	      (push `(map :from ,from-list
			  :through ,through-list
			  :to ,(name to))
		    mappings))))))
    (reply-to-msg message 'tell :content
      `(trips-ont-mappings
	 :mappings ,mappings
	 ))
    ))
(defcomponent-handler
    '(request &key :content (get-trips-ont-mappings . *))
     #'handle-get-trips-ont-mappings
  :subscribe t)

