;;;;
;;;; W::BARBECUE
;;;;

(define-words :pos W::n
 :words (
  (W::BARBECUE
  (senses
	   ((LF-PARENT ONT::DRESSINGS-SAUCES-COATINGS)
	    (TEMPL MASS-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::barbecue
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("cooking-45.3") :wn ("barbecue%2:30:00"))
     (LF-PARENT ONT::roast)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (EXAMPLE "John barbacued the pork ribs")
 ; like bake,blanch,boil,braise,cook,fry
     )
    )
   )
))

