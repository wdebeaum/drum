;;;;
;;;; W::enclose
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::enclose
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("contiguous_location-47.8") :wn ("enclose%2:35:00"))
     (LF-PARENT ONT::surround)
     (TEMPL neutral-neutral-xp-templ)
     (example "the fence encloses the porch")
     )
    ((meta-data :origin trips :entry-date 20090911 :change-date nil :comments nil :wn ("enclose%2:30:00" "enclose%2:35:01"))
     (LF-PARENT ONT::cover)
 ;    (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected2-optional-templ (xp (% w::pp (w::ptype w::in))))
     (example "enclose the check [in the envelope]")
     )
    )
   )
))

