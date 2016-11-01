;;;;
;;;; W::debug
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
 (W::debug
   (wordfeats (W::morph (:forms (-vb) :past W::debugged :ing W::debugging)))
   (SENSES
    ((EXAMPLE "debug the procedure")
     (LF-PARENT ONT::repair)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (meta-data :origin plot :entry-date 20080403 :change-date nil :comments nil)
     )
    )
   )
))

