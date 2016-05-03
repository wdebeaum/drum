;;;;
;;;; W::frame
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::frame
   (SENSES
    ((LF-PARENT ONT::info-medium)
     (EXAMPLE "reload the frame in your browser")
     (meta-data :origin task-learning :entry-date 20050816 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::frame
    (wordfeats (W::morph (:forms (-vb) :nom w::frame)))
   (SENSES
    ((LF-PARENT ONT::surround)
     (TEMPL neutral-neutral-xp-templ)
     (example "you hair frames you face")
     (meta-data :origin "verbnet-2.0" :entry-date 20060608 :change-date nil :comments nil :vn ("butter-9.9" "contiguous_location-47.8" "fill-9.8") :wn ("frame%2:35:00" "frame%2:42:00"))
     )
    )
   )
))

