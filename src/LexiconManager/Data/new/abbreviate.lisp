;;;;
;;;; W::abbreviate
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(W::abbreviate
   (wordfeats (W::morph (:forms (-vb) :nom w::abbreviation)))
   (SENSES
    ((meta-data :origin task-learning :entry-date 20050831 :change-date 20090504 :comments nil)
     (EXAMPLE "you can abbreviate the example as follows")
     (LF-PARENT ONT::language-adjust)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

