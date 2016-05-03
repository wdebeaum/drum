;;;;
;;;; W::fence
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::fence
   (SENSES
    ((meta-data :origin step :entry-date 20080827 :change-date nil :comments nil)
     (LF-PARENT ont::general-structure)
     (example "a 10 foot high fence")
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::fence
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("contiguous_location-47.8") :wn ("?fence%2:35:00"))
     (LF-PARENT ONT::surround)
     (TEMPL agent-affected-xp-templ) ; like cover,surround
     )
    )
   )
))

