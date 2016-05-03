;;;;
;;;; W::congest
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   (W::congest
    (wordfeats (W::morph (:forms (-vb) :nom w::congestion)))
   (SENSES
    ((LF-PARENT ONT::hindering)
     (meta-data :origin cardiac :entry-date 20090406 :change-date nil :comments nil)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected-xp-templ) 
     )
    )
   )
))

