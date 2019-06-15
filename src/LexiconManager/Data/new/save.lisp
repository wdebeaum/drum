;;;;
;;;; W::save
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::save
   (SENSES
    ((LF-PARENT ONT::rescue)
     (example "save the people")
     )
    ((LF-PARENT ONT::LEAVE-TIME)
     (example "it saves us five hours [to go along the coast]")
     (TEMPL THEME-AFFECTED-DURATION-TEMPL)
     )
    
    ((meta-data :origin plow :entry-date 20041122 :change-date nil :comments naive-subjects)
     (LF-PARENT ONT::save-cost)
     (example "it saved him five dollars to buy it on sale")
     (TEMPL EXPLETIVE-EXTENT-FORMAL-TEMPL)
     )
    ((LF-PARENT ONT::save-COST)
     (example "the ticket saved him five dollars")
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-BENEFICIARY-EXTENT-XP-TEMPL)
     )
    ((EXAMPLE "save the reservation/ save five dollars")
     (meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :comments nil)
     (LF-PARENT ONT::retain)
     (SEM (F::Cause F::agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

