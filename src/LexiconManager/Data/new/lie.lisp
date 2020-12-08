;;;;
;;;; W::LIE
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
;; nb ont::be-at-loc is trajectory - so e.g. "sitting at the window" doesn't parse as ont::be-at-loc
  (W::LIE
   (wordfeats (W::morph (:forms (-vb) :past W::lay :pastpart W::lain :ing W::lying)))
   (SENSES
    ((EXAMPLE "The stick was lying in the road")
     (LF-PARENT ONT::BE-AT-LOC)
     (SEM (F::Aspect F::Stage-level) (F::Time-span F::Extended))
     (TEMPL neutral-templ)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::LIE
   (wordfeats (W::morph (:forms (-vb) :past W::lied :ing W::lying)))
   (SENSES
    ((EXAMPLE "The patient lied")
     ;;(LF-PARENT ONT::announce)
     ;(lf-parent ont::assert)
     (lf-parent ont::misinform)
     (templ agent-templ)
     )
    (;;(LF-PARENT ONT::announce)
     ;(lf-parent ont::assert)
     (lf-parent ont::misinform)
     (example "he lied to her [about it]")
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-OPTIONAL-TEMPL 
	    (xp1 (% w::pp (w::ptype (? ptp w::to)))))
     )
    (;;(LF-PARENT ONT::announce)
     ;(lf-parent ont::assert)
     (lf-parent ont::misinform)
     (example "he spoke about it to her")
     (TEMPL AGENT-FORMAL-AGENT1-2-XP1-PP-3-XP2-PP-WITH-OPTIONAL-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  ((W::lie (W::down))
   (wordfeats (W::morph (:forms (-vb) :past W::lay :pastpart W::lain :ing W::lying)))
   (SENSES
    ((EXAMPLE "You had better lie down")
     (LF-PARENT ONT::body-movement-place) ;BODY-MOVEMENT-self)
     (LF-FORM W::lie-down)
     (TEMPL AGENT-TEMPL)
     )
    )
   )
))

