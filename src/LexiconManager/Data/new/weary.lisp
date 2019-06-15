;;;;
;;;; W::weary
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
	  (W::weary
	   (SENSES
	    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("weary%2:29:00"))
	     (LF-PARENT ONT::evoke-tiredness)
	     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
	     )
	    )
	   )
))

(define-words :pos W::adj :templ ADJ-EXPERIENCER-TEMPL
 :words (
   (W::weary
   (wordfeats (W::morph (:FORMS ( -LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090422 :change-date nil :comments nil :wn ("tired%3:00:00"))
     (lf-parent ont::fatigued-val)
     (templ central-adj-templ)
     )
    )
   )
))

