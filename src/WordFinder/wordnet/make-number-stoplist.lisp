;; USAGE: sbcl --load make-number-stoplist.lisp
;; writes number-stoplist.txt
;; NOTE: you must have already made $TRIPS_BASE/src/DeepSemLex/data/WordNet/COMPLETE
(load (make-pathname :directory '(:relative :up :up "config" "lisp")
		     :name "trips"))
(load #!TRIPS"src;DeepSemLex;code;lib;defsys")
(dfc:load-component :deepsemlex)
(in-package :dsl)
(defvar names
  (eval-path-expression
    '((/
	;; hyponyms of these nouns are literal numbers and number-like things,
	;; like ranks (mostly; see manual golist)
	( (/
	    WN::|digit%1:23:00::|
	    WN::|large_integer%1:23:00::|
	    WN::|simple_fraction%1:23:00::|
	    WN::|rank%1:26:00::|)
	  >inherit (+ (> WN::hyponym))
	  )
	;; satellites of this adjective are also literal numbers
	(1 WN::|cardinal%3:00:00::| >inherit (> WN::similar_to))
	)
      ;; get the names of all senses in these synsets (i.e. their sense keys)
      <inherit name #'symbol-name)))
(with-open-file (f "number-stoplist.txt"
		 :direction :output :if-exists :supersede)
  (format f "~{~a~%~}" (sort names #'string-lessp)))
(sb-ext:quit)
