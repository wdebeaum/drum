;;;;
;;;; w::emit
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
 (w::emit
  (wordfeats (W::morph (:forms (-vb) :nom w::emission)))
   (senses
    (
     (lf-parent ont::emit-giveoff-discharge) ;; 20120524 GUM change new parent
     (meta-data :origin LbR :entry-date 20080922 :change-date nil :comments nil :vn ("free-78-1"))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "the spark plug emits a spark")
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

