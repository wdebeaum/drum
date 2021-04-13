;;;;
;;;; W::transform
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::transform
    (wordfeats (W::morph (:forms (-vb) :nom w::transformation)))
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (LF-PARENT ont::transformation)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     ;(TEMPL AGENT-AFFECTED-RESULT-XP-PP-INTO-OPTIONAL-TEMPL (xp (% w::pp (w::ptype (? x w::into w::to)))))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments verbnet-expansion)
     (LF-PARENT ont::transformation)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     ;(TEMPL AFFECTED-RESULT-XP-OPTIONAL-TEMPL (xp (% w::pp (w::ptype (? x w::into w::to)))))
     (TEMPL AFFECTED-TEMPL)
     )
    )
   )
))
