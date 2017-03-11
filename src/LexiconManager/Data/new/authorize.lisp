;;;;
;;;; W::authorize
;;;;

(define-words :pos W::v 
 :words (
  (W::authorize
   (wordfeats (W::morph (:forms (-vb) :nom W::authorization)))
   (SENSES
    ((EXAMPLE "Authorize him to move")
     (LF-PARENT ONT::ALLOW)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-effect-affected-objcontrol-templ (xp (% W::cp (W::ctype W::s-to))))
     (meta-data :origin boudreaux :entry-date 20031024)
     )
    ((EXAMPLE "Authorize the purchase" "authorize the budget")
     (LF-PARENT ONT::allow)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected-xp-templ)
     (meta-data :origin calo :entry-date 20031206 :change-date nil :comments calo-y1script)
     )
    )
   )
))

