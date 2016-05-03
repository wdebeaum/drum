;;;;
;;;; w::soften
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (w::soften
   (senses
    ((meta-data :origin "verbnet-2.0" :entry-date 20060608 :change-date 20090505 :comments nil :vn ("other_cos-45.4") :wn ("soften%2:30:01" "soften%2:30:00"))
     (LF-PARENT ONT::sharpen-soft) ;; GUM change : new parent
     (example "soften the butter")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    ((meta-data :origin "wordnet-3.0" :entry-date 20090505 :change-date nil :comments nil)
     (LF-PARENT ONT::visual-adjust)
     (example "soften the image")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

