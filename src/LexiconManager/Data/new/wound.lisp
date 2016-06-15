;;;;
;;;; W::WOUND
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::WOUND
   (wordfeats (W::morph (:forms (-vb))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("wound%2:37:00"))
     (LF-PARENT ONT::evoke-injury)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (w::wound
  (senses
	   ((meta-data :wn ("wound%1:26:00"))
            (LF-PARENT ONT::wound)
	    (TEMPL count-pred-TEMPL)
	    )
	   )
)
))

