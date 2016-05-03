;;;;
;;;; w::slow
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
((w::slow (w::down))
 (senses
  ((meta-data :origin calo :entry-date 20041122 :change-date nil :comments caloy2)
   (LF-PARENT ONT::adjust)
   (example "that application will slow you down")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-affected-RESULT-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::to))))
   )
  )
 )
))

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
   ((W::slow (w::down))
     (wordfeats (W::morph (:forms (-vb) :nom (w::slow w::down))))
   (SENSES
    ((meta-data :origin coordops :entry-date 20070514 :change-date 20090504 :comments nil)
     (LF-PARENT ONT::decrease)
     (TEMPL agent-templ)
     (example "slow down")
     )
    ((meta-data :origin calo :entry-date 20070514 :change-date 20090504 :comments nil)
     (LF-PARENT ONT::decrease-speed)
     (TEMPL  agent-affected-xp-templ)
     (example "slow down the process")
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::SLOW
   (wordfeats (W::MORPH (:FORMS (-ER -LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("slow%3:00:01"))
     (LF-PARENT ONT::SPEED-VAL)
     (TEMPL LESS-ADJ-TEMPL)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("slow%3:00:01"))
     (LF-PARENT ONT::event-duration-modifier)
     )
    )
   )
))

