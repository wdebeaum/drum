;;;;
;;;; W::jerk
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::jerk
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("push-12-1"))
     (LF-PARENT ONT::pull)
     (TEMPL agent-affected-xp-templ) ; like pull
     )
    )
   )
))

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::jerk
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080222 :change-date nil :comments nil)
     (LF-PARENT ONT::uncontrolled-body-motion)
     (example "his leg jerked")
     (TEMPL affected-unaccusative-templ)
     )
    )
   )
))

