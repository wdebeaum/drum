;;;;
;;;; W::span
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::span
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("span%1:28:00"))
     (LF-PARENT ONT::time-interval)
     (TEMPL other-reln-templ)
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::span
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("contiguous_location-47.8") :wn ("span%2:42:00"))
     (LF-PARENT ONT::surround)
     (TEMPL neutral-neutral-xp-templ) ; like cover,surround
     )
    )
   )
))

