;;;;
;;;; W::invite
;;;;

(define-words :pos W::v 
 :words (
  (W::invite
    (wordfeats (W::morph (:forms (-vb) :nom w::invitation)))
   (SENSES
    ((EXAMPLE "He invited me to the beach." "I invited him for pizza.")
     (LF-PARENT ont::request)
     (templ AGENT-AFFECTED-NEUTRAL-TEMPL (xp (% W::pp (W::ptype (? xx W::to W::for)))))
     (SEM (F::Aspect F::bounded) (F::time-span F::atomic))
     )
    ((EXAMPLE "He invited me to go fishing.")
     (LF-PARENT ont::request)
     (templ AGENT-AFFECTED-THEME-OPTIONAL-TEMPL (xp (% W::cp (W::ctype W::s-to))))
     (SEM (F::Aspect F::bounded) (F::time-span F::atomic))
     )

    )
   )
))

