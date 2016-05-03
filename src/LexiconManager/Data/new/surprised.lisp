;;;;
;;;; w::surprised
;;;;

(define-words :pos W::adj 
  :templ CENTRAL-ADJ-TEMPL
 :words (
	  (w::surprised
	   (senses
	    ((lf-parent ont::surprised) (lf-form w::surprised)
	     (meta-data :origin bee :entry-date 20040608 :change-date nil :comments portability-experiment)
	     (templ central-adj-experiencer-templ)
	     )
	   #|| ((lf-parent ont::surprised)
	     (example "I am surprised that she does that")
	     (TEMPL ADJ-OF-CONTENT-XP-TEMPL)
	     )||#
	    ))
))

