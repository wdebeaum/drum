;;;;
;;;; W::truncate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
(W::truncate
   (wordfeats (W::morph (:forms (-vb) :nom w::truncation)))
   (SENSES
    ((meta-data :origin task-learning :entry-date 20050902 :change-date nil :comments nil)
     (EXAMPLE "you can truncate the example as follows")
     (LF-PARENT ONT::adjust)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

