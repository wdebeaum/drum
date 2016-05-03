;;;;
;;;; W::transform
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::transform
    (wordfeats (W::morph (:forms (-vb) :nom w::transformation)))
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (LF-PARENT ont::transformation)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected-result-optional-templ (xp (% w::pp (w::ptype (? w::into w::to)))))
     )
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments verbnet-expansion)
     (LF-PARENT ont::transformation)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL affected-result-optional-templ (xp (% w::pp (w::ptype (? w::into w::to)))))
     )
    )
   )
))
