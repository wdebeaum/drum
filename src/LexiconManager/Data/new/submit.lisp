;;;;
;;;; W::SUBMIT
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::SUBMIT
  (wordfeats (W::morph (:forms (-vb) :nom w::submission)))
   (SENSES
    ((EXAMPLE "submit the order")
     (LF-PARENT ONT::submit)
     (meta-data :origin plow :entry-date 20050328 :change-date nil :comments nil)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

