;;;;
;;;; W::listen
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
 (W::listen
  (wordfeats (W::morph (:forms (-vb) :past W::listened :ing W::listening)))
   (SENSES
    ((LF-PARENT ONT::ACTIVE-PERCEPTION)
     (SEM (F::Time-span F::extended))
     (TEMPL agent-NEUTRAL-xp-TEMPL  (xp (% W::pp (W::ptype W::to))))
     (example "listen to this")
     (meta-data :origin calo :entry-date 20041122 :change-date nil :comments caloy2)
     )
    ((LF-PARENT ONT::ACTIVE-PERCEPTION)
     (SEM (F::Time-span F::extended))
     (TEMPL agent-TEMPL)
     (example "you're not listening")
     (meta-data :origin calo :entry-date 20041122 :change-date nil :comments caloy2)
     )
    )
   )
))

