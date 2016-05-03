;;;;
;;;; W::decay
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::decay
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090504 :comments nil :vn ("entity_specific_cos-45.5") :wn ("decay%2:30:00" "decay%2:30:02"))
     (templ agent-affected-xp-templ)
     (LF-PARENT ONT::deteriorate)
 ; like ferment
     (syntax (w::resultative +))
     )
    ((meta-data :origin step :entry-date 20080623 :change-date 20090504 :comments nil)
     (LF-PARENT ONT::deteriorate)
     (example "the structure decayed")
     (templ affected-templ)
     (syntax (w::resultative +))
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

