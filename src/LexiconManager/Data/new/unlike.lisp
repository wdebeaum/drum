;;;;
;;;; w::unlike
;;;;

(define-words :pos w::adv
 :words (
;; Parentheticals
  (w::unlike
  (senses
	   ((TEMPL BINARY-CONSTRAINT-NP-TEMPL)
	    (LF-PARENT ONT::PARENTHETICAL)
	    (meta-data :origin beetle :entry-date 20090116 :change-date nil :comments nil)
	    (preference 0.98) ;; lower preference so that other senses are considered first, since this sense is very underconstrained
	    )
	   )
)
))

