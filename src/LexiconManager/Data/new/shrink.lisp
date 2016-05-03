;;;;
;;;; W::SHRINK
;;;;

(define-words :pos W::n
 :words (
  (W::SHRINK
  (senses
   ((LF-PARENT ONT::health-professional)
    (TEMPL COUNT-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::shrink
   (wordfeats (W::morph (:forms (-vb) :past W::shrank :pastpart W::shrunk :ing W::shrinking)))
   (SENSES
    ((LF-PARENT ONT::shrink)
     (EXAMPLE "I've shrunk the kids")
     (meta-data :origin "verbnet-2.0" :entry-date 20060608 :change-date 20090504 :comments nil :vn ("flinch-40.5" "other_cos-45.4") :wn ("shrink%2:30:02" "shrink%2:30:01" "shrink%2:30:00" "shrink%2:38:00"))
     )
    ((LF-PARENT ONT::shrink)
     (TEMPL THEME-TEMPL)
     (EXAMPLE "The kids shrank")
     (meta-data :origin "verbnet-2.0" :entry-date 20060608 :change-date 20090504 :comments nil :vn ("flinch-40.5" "other_cos-45.4") :wn ("shrink%2:30:02" "shrink%2:30:01" "shrink%2:30:00" "shrink%2:38:00"))
     )
    )
   )
))

