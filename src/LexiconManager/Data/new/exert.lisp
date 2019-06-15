;;;;
;;;; w::exert
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
 (w::exert
    (wordfeats (W::morph (:forms (-vb) :nom w::exertion)))
   (senses
    ((LF-PARENT ont::use)
     (meta-data :origin LbR :entry-date 20080922 :change-date nil :comments nil :vn ("free-78-1"))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "the amount of force exerted by the explosion")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

