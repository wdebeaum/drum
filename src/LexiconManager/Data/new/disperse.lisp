;;;;
;;;; w::disperse
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 ;; need to add a feature to restrict this to collective nouns, e.g. crowd (which is not plural)
  (w::disperse
   (senses
    ((meta-data :origin monroe :entry-date 20031217 :change-date nil :comments s15)
     (LF-PARENT ONT::disperse)
     (example "the crowd dispersed")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-plural-TEMPL)
     )
    ((meta-data :origin monroe :entry-date 20031217 :change-date nil :comments s15)
     (LF-PARENT ONT::disperse)
     (example "the police dispersed the crowd")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-affected-XP-TEMPL)
     )
    )
   )
))

