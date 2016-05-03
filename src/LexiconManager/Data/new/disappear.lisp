;;;;
;;;; W::disappear
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::disappear
   (wordfeats (W::morph (:forms (-vb) :nom W::disappearance)))
   (SENSES
    ((EXAMPLE "He disappeared")
     (LF-PARENT ONT::disappear) 
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL Affected-TEMPL)
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("disappearance-48.2") :wn ("disappear%2:30:00" "disappear%2:39:00" "disappear%2:30:02"))
     )
    )
   )
))

