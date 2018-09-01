;;;;
;;;; w::widen
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (w::widen
   (senses
    ((meta-data :origin "verbnet-2.0" :entry-date 20060606 :change-date 20090504 :comments nil :vn ("other_cos-45.4") :wn ("widen%2:30:00" "widen%2:30:05" "widen%2:30:03"))
     ;(LF-PARENT ONT::increase)
     (LF-PARENT ONT::widen)
     (example "widen the hole")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::width-scale))
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060606 :change-date 20090504 :comments nil :vn ("other_cos-45.4") :wn ("widen%2:30:00" "widen%2:30:05" "widen%2:30:03"))
     ;(LF-PARENT ONT::increase)
     (LF-PARENT ONT::widen)
     (TEMPL AFFECTED-unaccusative-TEMPL)
     (example "the river widens")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::width-scale))
     )
    )
   )
))

