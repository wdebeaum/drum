;;;;
;;;; W::crash
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::crash
    (wordfeats (W::morph (:forms (-vb) :nom w::crash)))
   (SENSES
    ((LF-PARENT ont::collide)
     (example "The car crashed into the wall" )
     (TEMPL affected-affected-templ (xp (% W::PP (W::ptype W::into))))
     )
    ((LF-PARENT ont::collide)
     (example "The cars crashed")
     (TEMPL affected-plural-templ)
     )
    ((LF-PARENT ONT::render-ineffective)
     (example "the computer crashed")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL affected-TEMPL)
     )
    #||((LF-PARENT ONT::BREAK-OBJECT)   ;; subsumed by CAUSE template
     (meta-data :origin calo :entry-date 20040908 :change-date nil :comments caloy2)
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "he crashed the computer")
     )||#
    ((LF-PARENT ont::render-ineffective)
     (meta-data :origin calo :entry-date 20040908 :change-date nil :comments caloy2)
     
     (TEMPL agent-affected-xp-templ)
     (example "it crashed the computer")
     )
    )
   )
))

