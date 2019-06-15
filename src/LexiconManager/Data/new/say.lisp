;;;;
;;;; W::say
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::say
   (wordfeats (W::morph (:forms (-vb) :past W::said)))
   (SENSES
    
    ((LF-PARENT ONT::SAY)
     (example "He said (to me) that three teams are going to Delta")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-FORMAL-2-XP-OPTIONAL-TEMPL (xp (% W::PP (w::ptype (? ptp w::to)))))
     )
    
    ((LF-PARENT ONT::SAY)
     (example "he said go over there")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% w::utt)))
     )
   
    ((LF-PARENT  ONT::say)
     (example "say it to him")
     (TEMPL AGENT-NEUTRAL-AGENT1-OPTIONAL-TEMPL)
     )

    (
     (LF-PARENT ONT::ASSERT)
     (example "Scientists say wooly mammoths are extinct.")
     (TEMPL AGENT-FORMAL-XP-CP-TEMPL) 
     )

    (
     (LF-PARENT ONT::encodes-message)
     (example "The book says that it is blue.")
     (TEMPL NEUTRAL-FORMAL-XP-NP-1-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     )

    (
     (LF-PARENT ONT::encodes-message)
     (example "The book says three years.")
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     )


    ((lf-parent ont::encodes-message)
     (Example "The book says to bake the potato.")
     (TEMPL NEUTRAL-FORMAL-CP-SUBJCONTROL-TEMPL)
     )

    ((lf-parent ont::locution)
     (example "Say this word to me.")
     (TEMPL AGENT-NEUTRAL-AGENT1-OPTIONAL-TEMPL)
    )

    ((lf-parent ont::command)
     (Example "I said to forget the whole thing.")
     (TEMPL AGENT-FORMAL-SUBJCONTROL-TEMPL (xp (% W::cp (W::ctype W::s-to))))
     )

    ((LF-PARENT ONT::SUGGEST)
     (example "I say we forgot the whole thing.")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-FORMAL-XP-CP-TEMPL (xp (% W::cp (W::ctype (? ctp W::s-finite w::s-that-subjunctive)))))
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

