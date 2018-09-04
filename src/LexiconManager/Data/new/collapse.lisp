;;;;
;;;; W::collapse
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::collapse
   (wordfeats (W::morph (:forms (-vb) :nom w::collapse))) 
   (SENSES
    ((LF-PARENT ONT::BREAK-OBJECT)
     (meta-data :origin "verbnet-2.0" :entry-date 20060519 :change-date nil :comments nil :vn ("other_cos-45.4") :wn ("collapse%2:38:00" "collapse%2:38:02" "collapse%2:38:03"))
     (example "the bridge collapsed")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL affected-TEMPL)
     )
    )
   )
))

