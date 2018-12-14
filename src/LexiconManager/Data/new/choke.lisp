;;;;
;;;; W::choke
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::choke
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
     (LF-PARENT ont::breathe)
     (TEMPL affected-templ) 
     (EXAMPLE "She choked with emotion as she talked about her deceased husband.")
     )
    ((meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
     (LF-PARENT ont::obstructed-breathing)
     (TEMPL affected-templ) 
     (EXAMPLE "She choked(gagged) on the fishbone.")
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  ((W::choke (w::up))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
     (LF-PARENT ont::breathe)
     (EXAMPLE "She choked up with emotion as she talked about her deceased husband.")
     (TEMPL affected-templ) ; like vomit
     )
    )
   )
))

