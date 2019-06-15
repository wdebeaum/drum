;;;;
;;;; W::bound
;;;;


(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::bound
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("contiguous_location-47.8") :wn ("bound%2:42:00"))
     (LF-PARENT ONT::surround)
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL) ; like cover,surround
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("bound%2:38:01"))
     (LF-PARENT ONT::self-locomote)
     (TEMPL agent-templ) ; like stroll,walk
     )
    )
   )
))

#|
(define-words :pos W::v
  :tags (:base500)
  :words (
   (W::bound
    (wordfeats (W::VFORM W::PASSIVE) (W::MORPH (:forms NIL)))
    (SENSES
     
     ((LF-PARENT ONT::ATTACH)
      (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
      (TEMPL agent-affected-as-comp-TEMPL (xp (% W::pp (W::ptype (? xxx W::to W::with)))))
      (example "It is bound to the stretcher")
      )

     ((LF-PARENT ONT::ATTACH)
      (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
      (TEMPL agent-plural-TEMPL)
      (example "They are bound together")
      )

     )
    ))
  )
|#
