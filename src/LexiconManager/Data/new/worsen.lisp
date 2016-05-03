;;;;
;;;; w::worsen
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
(w::worsen
 (wordfeats (W::morph (:forms (-vb) :ing w::worsening :past W::worsened)))
 (senses
   ((meta-data :origin cardiac :entry-date 20070809 :change-date 20090504 :comments nil)
    (LF-PARENT ONT::deteriorate)
    (example "have your symptoms worsened")
    (templ affected-templ)
   )
   ((meta-data :origin cardiac :entry-date 20081223 :change-date 20090504 :comments LM-vocab)
   (LF-PARENT ONT::deteriorate)
   (example "the medication worsened his condition")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL agent-affected-xp-templ)
   )
  )
 )
))

