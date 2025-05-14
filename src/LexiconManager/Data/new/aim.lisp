;;;;
;;;; W::aim
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::aim
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090511 :comments nil :vn ("wish-62"))
     ;(LF-PARENT ONT::intention)
     (LF-PARENT ONT::direct-at)
     (TEMPL NEUTRAL-FORMAL-XP-NP-2-TEMPL (xp (% w::cp (w::ctype w::s-to)))) ; like intend
     (PREFERENCE 0.96)
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090511 :comments nil :vn ("wish-62"))
     ;(LF-PARENT ONT::intention)
     (LF-PARENT ONT::direct-at)
     ;(TEMPL NEUTRAL-FORMAL-XP-NP-1-TEMPL (xp (% W::PP (W::ptype (? pt W::at W::for))))) 
     (TEMPL NEUTRAL-FORMAL-XP-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? pt W::at W::for))))) 
     (PREFERENCE 0.96)
     )

    ((LF-PARENT ONT::pointing-to)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL (xp (% W::PP (W::ptype (? pt W::toward w::towards))))) 
     (example "the triangle aims towards the square")
     (meta-data :origin calo-ontology :entry-date 20060215 :change-date nil :comments Orient)
     )
    ((LF-PARENT ONT::orient)
     (SEM (F::Aspect F::dynamic) (F::Time-span F::extended))
     ;(TEMPL agent-affected-GOAL-optional-TEMPL)
     (templ AGENT-AFFECTED-XP-NP-TEMPL)
     (example "aim the triangle (towards the square)")
     (meta-data :origin calo-ontology :entry-date 20060215 :change-date nil :comments Orient)
     )
    )
   )
))

