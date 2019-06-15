;;;;
;;;; W::TRAIL
;;;;

(define-words :pos W::n
 :words (
  ((W::TRAIL W::MIX)
  (senses
	   ((LF-PARENT ONT::CRACKERS)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::trail
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("chase-51.6") :wn ("trail%2:38:00"))
     (LF-PARENT ONT::pursue)
 ; like track
     )

    ((LF-PARENT ont::be-behind)
     (example "she trailed the frontrunner by 7000 votes")
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     )

    ((LF-PARENT ont::be-behind)
     (example "they are trailing" "the candidate trailed in the polls") 
     (TEMPL neutral-templ)
    )

    )
   )
))

