;;;;
;;;; W::match
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (W::match
   (SENSES
    ((EXAMPLE "it doesn't match your specifications")
     (LF-PARENT ONT::resemble)
     (SEM (F::Aspect F::static) (F::Time-span F::extended))
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     (meta-data :origin calo :entry-date 20031206 :change-date nil :comments calo-y1script)
     )

    ((EXAMPLE "his story does not match with her story")
     (LF-PARENT ONT::resemble)
     (SEM (F::Aspect F::static) (F::Time-span F::extended))
     (TEMPL neutral-neutral1-xp-templ (xp (% W::pp (W::ptype W::with)))) 
    )
    
    ((EXAMPLE "the two stories don't match exactly")
     (LF-PARENT ONT::resemble)
     (SEM (F::Aspect F::static) (F::Time-span F::extended))
     (TEMPL neutral-templ)
    ) 

    )
   )
))

