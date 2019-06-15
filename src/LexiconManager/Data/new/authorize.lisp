;;;;
;;;; W::authorize
;;;;

(define-words :pos W::v 
 :words (
  (W::authorize
   (wordfeats (W::morph (:forms (-vb) :nom W::authorization)))
   (SENSES
    ((EXAMPLE "Authorize him to move")
     ;(LF-PARENT ONT::ALLOW)
     (LF-PARENT ONT::approve-authorize)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-FORMAL-CP-OBJCONTROL-TEMPL (xp (% W::cp (W::ctype W::s-to))))
     (meta-data :origin boudreaux :entry-date 20031024)
     )
    ((EXAMPLE "Authorize the purchase" "authorize the budget")
     ;(LF-PARENT ONT::allow)
     (LF-PARENT ONT::approve-authorize)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (meta-data :origin calo :entry-date 20031206 :change-date nil :comments calo-y1script)
     )
    )
   )
))

