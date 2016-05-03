(in-package :dsl)
;; copied from WebParser, which was in turn copied from LFEvaluator...
;; TODO make a universal version of this, put it in src/util/ ?

(defun convert-one-package (expr from-pkg &optional (to-pkg *package*))
  "Convert symbols in from-pkg to to-pkg, while leaving other symbols alone. See also ../lib/load.lisp:ld-to-dsl-package and util:convert-to-package."
  (unless (packagep from-pkg)
    (setf from-pkg (find-package from-pkg)))
  (cond
    ((null expr) expr)
    ((consp expr)
      (cons (convert-one-package (car expr) from-pkg to-pkg)
            (convert-one-package (cdr expr) from-pkg to-pkg)))
    ((and (symbolp expr) (eq (symbol-package expr) from-pkg))
      (intern (symbol-name expr) to-pkg))
    (t expr)
    ))

(defun lf-to-rdf-stream (lf-terms s)
  "Write RDF representing the given LF terms to the given stream."
  (dolist (str '(
      "~&~vt<rdf:RDF"
      "~&~vt    xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\""
      "~&~vt    xmlns:role=\"http://www.cs.rochester.edu/research/trips/role#\""
      "~&~vt    xmlns:LF=\"http://www.cs.rochester.edu/research/trips/LF#\">"
      ))
    (format s str *indent*))
  (incf *indent*)
  (dolist (term lf-terms)
    (let ((indicator (convert-to-package (first term)))
	  (id (convert-to-package (second term)))
	  (type-word (third term))
	  (roles (cdddr term))
	  type word)
      ;; interpret the different kinds of third element
      (if (consp type-word)
	(cond
	  ((eq :* (car type-word))
	    (setf type (second type-word))
	    (setf word (convert-one-package (third type-word) :w))
	    )
	  ((eq '<OR> (convert-to-package (car type-word)))
	    (setf type type-word))
	  ;; FIXME does this even happen anymore?
	  ((eq 'set-of (convert-to-package (car type-word)))
	    (setf type 'set)
	    (setf roles (append (list :of (second type-word)) roles))
	    )
	  (t
	    (error "Unknown type of third element of a term: ~s" type-word))
	  )
	;; not a list
	(setf type type-word)
	)
      (format s "~&~vt<rdf:Description rdf:ID=\"~s\">" *indent* id)
      (indented
	(format s "~&~vt<LF:indicator>~s</LF:indicator>" *indent* indicator)
	(format s "~&~vt<LF:type>~s</LF:type>" *indent* type)
	(when word
	  (format s "~&~vt<LF:word>~s</LF:word>" *indent* word))
	(loop while roles
	      for role = (pop roles) then (pop roles)
	      for value = (pop roles) then (pop roles)
	      do
	  (cond
	    ((eql :TMA role)
	      ;; expand TMA into separate roles
	      (dolist (pair (reverse value))
		(push (second pair) roles)
		(push (intern (symbol-name (first pair)) :keyword) roles)
		))
	    ((and (consp value) (every #'is-trips-variable value))
	      (format s "~&~vt<role~s><rdf:Seq>" *indent* role)
	      (indented
		(dolist (m (convert-to-package value))
		  (format s "~&~vt<rdf:li rdf:resource=\"#~s\" />" *indent* m)))
	      (format s "~&~vt</rdf:Seq></role~s>" *indent* role)
	      )
	    ((eql :WNSENSE role)
	      ;; prevent WN synset IDs being interpreted as term IDs, and
	      ;; handle AND/OR lists
	      (if (and (consp value)
		       (member (symbol-name (car value)) '("AND" "OR")
			       :test #'string=))
		(format s "~&~vt<role~s>~(<~a>~{~s~^ ~}</~a>~)</role~s>"
			*indent* role (car value) (cdr value) (car value) role)
		; single symbol
	        (format s "~&~vt<role~s>~s</role~s>" *indent* role value role)
		))
	    ((is-trips-variable value)
	      ;; write the role as a resource reference
	      (format s "~&~vt<role~s rdf:resource=\"#~s\" />" *indent* role (convert-to-package value)))
	    ;; TODO separate out parts of role values like
	    ;; (:* length-unit meter) for :UNIT and (:* possibility may) for
	    ;; :modality
	    (t
	      ;; write the role as a string
	      (format s "~&~vt<role~s>~s</role~s>" *indent* role value role))
	    ))
	)
      (format s "~&~vt</rdf:Description>" *indent*)
      ))
  (decf *indent*)
  (format s "~&~vt</rdf:RDF>" *indent*)
  )

