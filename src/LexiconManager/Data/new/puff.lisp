;;;;
;;;; w::puff
;;;;

(define-words :pos W::n
 :words (
  (w::puff
  (senses
   ((LF-PARENT ont::substance-delivery-unit) 
    (meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("pill%1:06:00"))
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::n
 :words (
  ((W::PUFF w::pastry)
  (senses
	   ((LF-PARENT ONT::CAKE-PIE)
	    (TEMPL MASS-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::puff (w::up))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090504 :comments LM-vocab :vn ("entity_specific_cos-45.5"))
     (LF-PARENT ONT::swell)
     (SYNTAX (w::resultative +))
     (examples "his ankles puffed up")
     (templ affected-unaccusative-templ)
     )
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090504 :comments LM-vocab :vn ("entity_specific_cos-45.5"))
     (LF-PARENT ONT::swell)
     (SYNTAX (w::resultative +))
     (preference .97) ;; prefer intransitive
     (examples "fluid puffs his stomach up")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::puff
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090504 :comments LM-vocab :vn ("entity_specific_cos-45.5"))
     (LF-PARENT ONT::swell)
     (SYNTAX (w::resultative +))
     (examples "the rice puffed")
     (templ affected-unaccusative-templ)
     )
    )
   )
))

