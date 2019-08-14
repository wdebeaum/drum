;;;;
;;;; W::stick
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
 (W::stick
   (SENSES
    ((LF-PARENT ONT::quantity)
     (TEMPL classifier-mass-templ)
     (example "a stick of butter/wood")
     (meta-data :origin foodkb :entry-date 20050809 :change-date 20090520 :comment nil)
     )
    ((LF-PARENT ONT::natural-object)
     (TEMPL count-pred-templ)
     (example "a stick of butter/wood")
     (meta-data :origin foodkb :entry-date 20050809 :change-date 20090520 :comment nil)
     )
   ))
))

(define-words :pos W::v 
 :words (
  (W::stick
   (wordfeats (W::morph (:forms (-vb) :past W::stuck)))
   (SENSES
    ((example "let's stick with the mac")
     (LF-PARENT ONT::retain)
     ;;(SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-FORMAL-PRED-SUBJCONTROL-TEMPL)
     (meta-data :origin calo :entry-date 20041122 :change-date nil :comments caloy2)
     )
    ((example "it stuck")
     (meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("exist-47.1-1"))
     (LF-PARENT ONT::stay)
     (TEMPL agent-templ)
     )
    
    ((EXAMPLE "he stuck me in the ribs")
     (meta-data :origin calo-ontology :entry-date 20051213 :change-date nil :comments Penetrate)
     (LF-PARENT ONT::penetrate)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )

    ((LF-PARENT ONT::ATTACH)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL (xp (% W::pp (W::ptype W::to))))
     )

    ((LF-PARENT ONT::put)
     (meta-data :origin vn-analysis :entry-date unknown :change-date 20040617 :comments change-lf)
     (example "stick the box in the corner")
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-RESULT-ADVBL-OBJCONTROL-TEMPL)
     )
    
    
    ))
  ))

