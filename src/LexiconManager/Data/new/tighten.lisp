;;;;
;;;; w::tighten
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (w::tighten
   (senses
    ((meta-data :origin "verbnet2.0" :entry-date 20060606 :change-date 20090504 :comments nil :vn ("other_cos-45.4") :wn ("tighten%2:30:01" "tighten%2:30:00"))
     ;(LF-PARENT ONT::increase)
     (LF-PARENT ONT::ADJUST)
     (syntax (w::resultative +)) ;; the tightened screw
     (example "tighten the screw")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    ((meta-data :origin "verbnet2.0" :entry-date 20060606 :change-date 20090504 :comments nil :vn ("other_cos-45.4") :wn ("tighten%2:30:01" "tighten%2:30:00"))
     ;(LF-PARENT ONT::increase)
     (LF-PARENT ONT::ADJUST)
     (templ affected-templ)
     (example "the screws tightened")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

