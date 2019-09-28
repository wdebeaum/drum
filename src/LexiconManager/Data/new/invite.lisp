;;;;
;;;; W::invite
;;;;

(define-words :pos W::v 
 :words (
  (W::invite
    (wordfeats (W::morph (:forms (-vb) :nom w::invitation)))
   (SENSES
    ((EXAMPLE "he invited me to the beach" "I invited him for pizza" "the community warmly invites the refugees")
     (LF-PARENT ont::request)
     (TEMPL AGENT-AFFECTED-NEUTRAL-XP-TEMPL (xp (% W::pp (W::ptype (? xx W::to W::for)))))
     (SEM (F::Aspect F::bounded) (F::time-span F::atomic))
     )
    ((EXAMPLE "He invited me to go fishing.")
     (LF-PARENT ont::request)
     (TEMPL AGENT-AFFECTED-FORMAL-XP-OPTIONAL-A-TEMPL (xp (% W::cp (W::ctype W::s-to))))
     (SEM (F::Aspect F::bounded) (F::time-span F::atomic))
     )
   
;    ((EXAMPLE "The community warmly invites the refugees.")
;     (LF-PARENT ont::admit)
;     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
;     ) 

    )
   )
))

