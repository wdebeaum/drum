;;;;
;;;; W::manage
;;;;

(define-words :pos W::v 
 :words (
   (W::manage
    (wordfeats (W::morph (:forms (-vb) :nom W::management)))
   (SENSES
    ((lf-parent ont::achieve)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "he managed to solve the problem")
     (TEMPL agent-effect-subjcontrol-templ (xp (% w::cp (w::ctype w::s-to))))
     (meta-data :origin calo :entry-date 20041123 :change-date nil :comments caloy2)
     )
     (;;(lf-parent ont::managing)
      (lf-parent  ont::guiding) ;; 20120521 GUM change new parent 
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "he manages the project")
     (TEMPL agent-affected-xp-templ)
     (meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil)
     )
    ))
))

