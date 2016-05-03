(mapcar (lambda (p) (unless (find-package p) (make-package p :use nil)))
        '(ont f w lexiconmanager))

(defun getk (l k)
  (second (member k l)))

(defun remove-keyword-arg (form key)
  "@returns a copy of form without the first occurence of key and its value
   @example (remove-keyword-arg '(:him us :her you hi) :him) -> (:HER YOU HI)
   @author Nate Chambers"
  (cond ((null form) nil)
        ((atom form) (list form))
        ((eq key (first form)) (cddr form))
        (t (cons (first form) (remove-keyword-arg (rest form) key)))
	))

(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
    x-y
    (cons x y)
    ))

(defun map-tree (pred fn tree)
  "Return a modified tree, where all nodes for which pred is true are replaced with the result of passing them to fn (and not traversed further)."
  (cond
   ((funcall pred tree)
    (funcall fn tree))
   ((consp tree)
    (reuse-cons (map-tree pred fn (car tree))
                (map-tree pred fn (cdr tree))
		tree))
   (t
    tree)
   ))

(defun convert-to-package (x &optional (package *package*) &key (convert-keywords nil))
  "Converts non-keyword symbols (or keyword symbols as well if
   :convert-keywords is true) in the given object to the given package, default
   *PACKAGE*."
  (map-tree
    (lambda (tree)
      (and (not (null tree))
           (symbolp tree)
           (or convert-keywords
	       (not (keywordp tree)))))
    (lambda (sym)
      (intern (symbol-name sym) package))
    x))

;; TODO move map-tree to src/util/util.lisp and use that here

(defun slurp-file (f)
  "read the file named f into a string and return it"
  (with-open-file (s f)
    (let ((str (make-string (file-length s))))
      (read-sequence str s)
      str)))

;; how is this not defined in the spec???
(defun whitespace-char-p (c)
  "Is c a whitespace character?"
  (consp (member c (list #\Space #\Newline #\Tab #\Return))))

(defun read-dot-id (s &key (followed-by (list #'whitespace-char-p)))
  "Read a dot ID.
   @param followed-by A list of characters/predicates allowed to follow the ID."
  (if (eql #\" (peek-char t s))
    (read s) ; it's a string, just read it
    ; not a string, need to get everything up to a followed-by char
    (let* ((chars
             (loop until (some (lambda (fb)
				 (if (characterp fb)
				   (eql fb (peek-char nil s))
				   (funcall fb (peek-char nil s))))
			       followed-by)
	           collect (read-char s)
		   ))
            (str (concatenate 'string chars)))
      (handler-case (read-from-string str)
        (error (e) (error "Error reading dot ID from string ~s: ~a" str e))))
    ))

(defun destringify-dot-id (id)
  "Make sure a dot ID isn't left as a string, by reading it as a lisp
   expression."
  (if (stringp id)
    (handler-case (read-from-string id)
      (error (e) (error "Error reading lisp expression from dot ID ~s: ~a" id e)))
    id))

(defun read-dot-attribute (s)
  "read a single dot attribute"
  (let ((key (read-dot-id s :followed-by (list #\=)))
        (equals-sign (read-char s))
        (value (read-dot-id s :followed-by (list #\; #\]))))
      (declare (ignore equals-sign))
    (cons key value)))

(defun read-dot-attributes (s)
  "read a bracketted list of dot attributes"
  (let ((attrs (loop until (eql #\] (peek-char t s))
  		     do (read-char s) ; discard [ or ;
		     collect (read-dot-attribute s)
		     )))
    (read-char s) ; discard ]
    attrs))

(defun read-dot-graph-element (s)
  "read a single dot graph element (node or edge)"
  (let (e)
    ;; read the (from) node ID
    (push (read-dot-id s :followed-by (list #\- #\[ #'whitespace-char-p)) e)
    (when (eql #\- (peek-char t s))
      ;; read the edge type (-- or ->)
      (push (intern (concatenate 'string (list (read-char s) (read-char s)))) e)
      ;; read the to node ID
      (push (read-dot-id s :followed-by (list #\[)) e)
      )
    (when (eql #\[ (peek-char t s))
      (push (read-dot-attributes s) e))
    ;; undo the reversal of push
    (reverse e)))

(defun read-dot-graph (s)
  "Read a Graphviz dot graph from the stream s, and return a list representation
   of it."
  (let (g)
    ;; get the graph type and name
    (push (read-dot-id s) g)
    (push (read-dot-id s) g)
    ;; discard {
    (peek-char #\{ s)
    (read-char s)
    ;; read graph elements until the close brace
    (loop until (eql #\} (peek-char t s))
          do (push (read-dot-graph-element s) g))
    ;; discard }
    (read-char s)
    ;; undo the reversal of push
    (reverse g)))

(defun add-to-plural-arg (id2term term-id plural-key value)
  "Add value to the list of values for the given plural-key on the given term.
   Create the list if necessary."
  (let ((term (gethash term-id id2term)))
    (cond
      ((null term) ; don't have the term yet, add the arg to a new term
	(setf (gethash term-id id2term)
	      (list plural-key (list value))))
      ((not (member plural-key term)) ; don't have the arg, add it
	(setf (gethash term-id id2term)
	      (append term
		      (list plural-key (list value)))))
      (t ; already have this arg, just add to it
        (let ((prev-value (second (member plural-key term))))
	  (if (not (listp prev-value))
	    (error "Expected list in plural keyword argument ~S, but got ~S (edge labels in the original graph need to be singular)" plural-key prev-value)
	    ; no error
	    (setf (second (member plural-key term))
		  (append prev-value (list value)))
	    )))
      )))

(defun fix-lf-packages (lf)
  "Fix the packages of the symbols in the LF (which can be a single term or a
   list of them); Everything goes into the ONT package, except keywords and the
   third element of lists of the form (:* symbol symbol)."
  (map-tree
    (lambda (tree)
      (and (listp tree)
           (= 3 (length tree))
           (eq :* (first tree))
	   (symbolp (second tree))
	   (symbolp (third tree))))
    (lambda (lftype)
      (list :* (second lftype) (convert-to-package (third lftype) :w)))
    (convert-to-package lf :ont)
    ))

(defun convert-dot-to-terms (g)
  "Convert a dot graph as read by read-dot-graph to a list of LF terms.
   @param g Can be either the string version of the graph or the list version
   that read-dot-graph returns."
  (let ((id2term (make-hash-table))
        (id2nonterm (make-hash-table)))
    (dolist (e (cddr (if (stringp g)
                       (with-input-from-string (s g) (read-dot-graph s))
		       g)))
      (cond
        ((member (first e) '(node edge graph) :key #'destringify-dot-id)
	  nil) ; ignore global attributes
	((eql '-> (second e)) ; edge
	 (let ((node-id (destringify-dot-id (first e)))
	       (key (destringify-dot-id (cdr (assoc 'label (fourth e) :key #'destringify-dot-id))))
	       (value (destringify-dot-id (third e))))
	   (cond
	     ((member key '(:mod :member)) ; => :mods, :members
	       (let ((plural-key (intern (concatenate 'string (string key) "S") 'keyword)))
		 (add-to-plural-arg id2term node-id plural-key value)))
	     ((member key '(:and-element :or-element)) ; => :and, :or
	       (let* ((singular-key-name (symbol-name key))
	              (plural-key-length (- (length singular-key-name) 8))
	              (plural-key
		        (intern
			  (subseq singular-key-name 0 plural-key-length)
			  'keyword)))
		 (add-to-plural-arg id2term node-id plural-key value)))
	     ; TODO :domain-info special case?
	     (t ; not a plural key, just add it
	       (setf (gethash node-id id2term) 
		     (append (gethash node-id id2term) 
			     (list key value))))
	     )))
	((and (stringp (first e))
	      (find-if-not
	        (lambda (c)
		  (or (alphanumericp c) (member c (list #\- #\_ #\* #\. #\: #\^))))
		(first e)))
	 ; string containing non-id characters
	 ;; ignore, because it's probably the input string
	 nil)
	(t ; node
	 (let ((node-id (destringify-dot-id (first e)))
	       (label (destringify-dot-id (cdr (assoc 'label (second e) :key #'destringify-dot-id)))))
	   (cond
	     ;; is the label a list starting with an LF term constructor?
	     ((and (listp label) (= 3 (length label))
		   (member (intern (symbol-name (first label)))
			   '(
			     ;; these were all seen in a test on 2011-08-09
			     a bare f impro indef-set kind pro pro-set quantifier sa-seq sm speechact the the-set value wh-term
			     ;; these were here before, but weren't seen in the test
			     quantity-term quan conjunct
			     )))
	       ; a normal term
	       (setf (gethash node-id id2term)
		     (append label (gethash node-id id2term))))
	     ; otherwise, an argument value that isn't a term
	     (label ; node has a label, but it's not a term
	       ;; store the id->label mapping so we can change all instances of
	       ;; the id to the label later
	       (setf (gethash node-id id2nonterm) label))
	     (t ; the node doesn't have a label, so we can leave its "id" in
	        ; place, and just not add this node as a term
	       nil)
	     )))
	))
    ;; convert term node IDs to lisp term IDs, and non-term IDs to their
    ;; associated labels
    (maphash (lambda (k v)
               (setf (gethash k id2term)
	             (mapcar (lambda (el)
		               (cond
			        ;; list of term IDs
				((and (listp el)
				      (every
				        (lambda (elel)
					  (nth-value 1 (gethash elel id2term)))
					el))
				 (mapcar
				   (lambda (elel)
				     (second (gethash elel id2term)))
				   el))
				;; single term ID
				((nth-value 1 (gethash el id2term))
				 (second (gethash el id2term)))
				;; non-term node ID
		                ((nth-value 1 (gethash el id2nonterm))
			         (gethash el id2nonterm))
				;; not an ID
				(t
				 el)))
		             v)))
             id2term)
    ;; return the list of terms, with the right packages
    (fix-lf-packages (loop for v being the hash-values in id2term collect v))
    ))
