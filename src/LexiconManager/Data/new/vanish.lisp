;;;;
;;;; W::vanish
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::vanish
   (SENSES
    ((EXAMPLE "He vanished")
     (LF-PARENT ONT::disappear) ; like disappear
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL Affected-TEMPL)
     (meta-data :origin "verbnet-2.0" :entry-date 20060606 :change-date nil :comments nil :vn ("disappearance-48.2") :wn ("vanish%2:30:00" "vanish%2:39:00" "vanish%2:38:05" "vanish%2:30:02" "vanish%2:30:01"))
     )
    )
   )
))

