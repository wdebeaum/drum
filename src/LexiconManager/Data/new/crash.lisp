;;;;
;;;; W::crash
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::crash
    (wordfeats (W::morph (:forms (-vb) :nom w::crash)))
   (SENSES
    ((LF-PARENT ont::collide)
     (example "The car crashed into the wall" )
     (TEMPL AFFECTED-AFFECTED1-XP-NP-TEMPL (xp (% W::PP (W::ptype W::into))))
     )
    ((LF-PARENT ont::collide)
     (example "The cars crashed")
     (TEMPL AFFECTED-NP-PLURAL-TEMPL)
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
     
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "it crashed the computer")
     )
    )
   )
))

