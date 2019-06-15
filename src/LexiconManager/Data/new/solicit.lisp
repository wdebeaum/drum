;;;;
;;;; W::solicit
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::solicit
   (wordfeats (W::morph (:forms (-vb) :nom W::solicitation)))
   (SENSES
    ((EXAMPLE "solicit your approval")
     (LF-PARENT ONT::appeal-apply-demand)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (meta-data :origin calo :entry-date 20060524 :change-date nil :comments pq0149)
     )
    )
   )
))

