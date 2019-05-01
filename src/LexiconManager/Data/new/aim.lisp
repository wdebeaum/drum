;;;;
;;;; W::aim
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::aim
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090511 :comments nil :vn ("wish-62"))
;     (LF-PARENT ONT::intention)
     (LF-PARENT ONT::direct-at)
     (TEMPL neutral-theme-xp-templ (xp (% w::cp (w::ctype w::s-to)))) ; like intend
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::pointing-to)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL neutral-neutral-xp-templ (xp (% W::PP (W::ptype (? pt W::toward w::towards))))) 
     (example "the triangle aims towards the square")
     (meta-data :origin calo-ontology :entry-date 20060215 :change-date nil :comments Orient)
     )
    ((LF-PARENT ONT::orient)
     (SEM (F::Aspect F::dynamic) (F::Time-span F::extended))
     (TEMPL agent-affected-GOAL-optional-TEMPL)
     (example "aim the triangle (towards the square)")
     (meta-data :origin calo-ontology :entry-date 20060215 :change-date nil :comments Orient)
     )
    )
   )
))

