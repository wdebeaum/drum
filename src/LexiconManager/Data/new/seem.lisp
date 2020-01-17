;;;;
;;;; W::seem
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  ((W::seem W::like)
   (SENSES
    ;;;; it seems like we will have to go
    ((LF-PARENT ONT::POSSIBLY-true)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL EXPLETIVE-NEUTRAL-1-XP1-2-XP2-TEMPL (xp1 (% W::NP (W::lex W::it))) (xp2 (% W::s (W::stype W::decl))))
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::seem
   (SENSES
    ((LF-PARENT ONT::APPEARS-TO-HAVE-PROPERTY)
     (example "he seems happy")
     (TEMPL NEUTRAL-FORMAL-PRED-SUBJCONTROL-TEMPL)
     (SYNTAX (w::exclude-passive +))
     )
    ((EXAMPLE "It seems that he came")
     (LF-PARENT ONT::POSSIBLY-true)
     ;(LF-PARENT ONT::APPEARS-TO-HAVE-PROPERTY)
     (SEM (F::Aspect F::stage-level))
     (TEMPL EXPLETIVE-NEUTRAL-1-XP1-2-XP2-TEMPL (xp1 (% W::NP (W::lex W::it))) (xp2 (% W::cp (W::ctype W::s-finite))))
     (SYNTAX (w::exclude-passive +))
     )
    ((EXAMPLE "there seems to be a truck on the corner")
     ;(LF-PARENT ONT::POSSIBLY-true)
     (LF-PARENT ONT::APPEARS-TO-HAVE-PROPERTY)
     (SEM (F::Aspect F::stage-level))
     (TEMPL EXPLETIVE-NEUTRAL-1-XP1-2-XP2-TEMPL (xp1 (% W::NP (W::lex W::there))) (xp2 (% W::cp (W::ctype W::s-to))))
     (SYNTAX (w::exclude-passive +))
     )
    ((LF-PARENT ONT::APPEARS-TO-HAVE-PROPERTY)
     (example "he seems to be happy/the situation seems to be out of control")
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-FORMAL-CP-SUBJCONTROL-TEMPL (xp (% W::cp (W::ctype W::s-to))))
     (SYNTAX (w::exclude-passive +))
     )
    )
   )
))

