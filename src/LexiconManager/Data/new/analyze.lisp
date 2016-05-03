;;;;
;;;; W::analyze
;;;;

(define-words :pos W::v 
 :words (
  (W::analyze
   (wordfeats (W::morph (:forms (-vb) :nom w::analysis)))
   (SENSES
    ((meta-data :origin task-learning :entry-date 20050829 :change-date nil :comments nil)
     (LF-PARENT ONT::scrutiny)
     (EXAMPLE "analyze the email")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-xp-templ)
     )
    )
   )
))

