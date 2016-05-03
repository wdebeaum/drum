;;;;
;;;; W::speak
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::speak
   (wordfeats (W::morph (:forms (-vb) :past W::spoke :pastpart W::spoken)))
   (SENSES
    ;;;;
    (;;(LF-PARENT ONT::talk)
     (LF-PARENT  ONT::say)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     )
    ;;;; he spoke with/to her about it
    (;;(LF-PARENT ONT::TALK)
     (LF-PARENT  ONT::talk)
     (example "he spoke to/with her [about it]")
     (TEMPL AGENT-ADDRESSEE-THEME-OPTIONAL-TEMPL 
	    (xp1 (% w::pp (w::ptype (? ptp w::to w::with)))))
     )
;;;; he spoke about it to/with her
    (;;(LF-PARENT ONT::TALK)
     (LF-PARENT  ONT::talk)
     (example "he spoke about it [to/with her]")
     (TEMPL AGENT-ABOUT-THEME-ADDRESSEE-OPTIONAL-TEMPL)
     )
     ;;;; he spoke at 5 pm
    (;;(LF-PARENT ONT::TALK)
     (LF-PARENT  ONT::say)
     (TEMPL AGENT-TEMPL)
     )
    )
   )
))

