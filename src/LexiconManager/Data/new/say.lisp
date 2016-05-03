;;;;
;;;; W::say
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::say
   (wordfeats (W::morph (:forms (-vb) :past W::said)))
   (SENSES
    
    ((LF-PARENT ONT::SAY)
     (example "He said (to me) that three teams are going to Delta")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-affected-xp-optional-formal-TEMPL (xp (% W::PP (w::ptype (? ptp w::to)))))
     )
    
    ((LF-PARENT ONT::SAY)
     (example "he said go over there")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-THEME-XP-TEMPL (xp (% w::utt)))
     )
   
    ((LF-PARENT  ONT::say)
     (example "say it to him")
     (TEMPL AGENT-neutral-to-addressee-optional-TEMPL)
     )

    (
     (LF-PARENT ONT::ASSERT)
     (example "Scientists say wooly mammoths are extinct.")
     (TEMPL agent-formal-xp-templ) 
     )

    (
     (LF-PARENT ONT::encodes-message)
     (example "The book says that it is blue.")
     (TEMPL neutral-formal-as-comp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )

    (
     (LF-PARENT ONT::encodes-message)
     (example "The book says three years.")
     (TEMPL neutral-neutral-xp-templ)
     )

    ((lf-parent ont::encodes-message)
     (Example "The book says to bake the potato.")
     (TEMPL agent-effect-subjcontrol-templ (xp (% W::cp (W::ctype W::s-to))))
     )

    ((lf-parent ont::locution)
     (example "Say this word to me.")
     (TEMPL AGENT-neutral-to-addressee-optional-TEMPL)
    )

    ((lf-parent ont::command)
     (Example "I said to forget the whole thing.")
     (TEMPL agent-effect-subjcontrol-templ (xp (% W::cp (W::ctype W::s-to))))
     )

    ((LF-PARENT ONT::SUGGEST)
     (example "I say we forgot the whole thing.")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-formal-XP-TEMPL (xp (% W::cp (W::ctype (? ctp W::s-finite w::s-that-subjunctive)))))
     )


    )
   )
))

#|
(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  ((W::SAY (W::OVER))
   (wordfeats (W::morph (:forms (-vb) :past W::said)))
   (SENSES
    ;;;; swier -- say the plan over.
    ((LF-PARENT ONT::REPEAT)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  ((W::SAY (W::AGAIN))
   (wordfeats (W::morph (:forms (-vb) :past W::said)))
   (SENSES
    ((LF-PARENT ONT::REPEAT)
     )
    )
   )
))
|#

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((w::say W::again)
   (SENSES
    ((LF (W::HUH))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_NOLO-COMPRENDEZ))
     (preference .97) 
     )
    )
   )
))

