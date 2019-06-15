;;;;
;;;; W::obstruct
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::obstruct
   (wordfeats (W::morph (:forms (-vb) :nom w::obstruction)))
   (SENSES
    ((LF-PARENT ONT::hindering)
     (meta-data :origin cardiac :entry-date 20090403 :change-date nil :comments nil)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) 
     )
    )
   )
))

