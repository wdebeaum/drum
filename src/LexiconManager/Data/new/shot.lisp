;;;;
;;;; W::shot
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::shot
   (SENSES
     ((meta-data :origin cardiac :entry-date 20090402 :change-date nil :comments nil)
     (LF-PARENT ont::medical-event)
     (example "he got a flu shot at the clinic")
       )
    ((meta-data :origin  medadvisor :entry-date 20060803 :change-date nil :comments nil :wn ("shot%1:23:00"))
     (LF-PARENT ONT::small-container)
     (example "a shot of vodka")
     )
    ((LF-PARENT ONT::image)
     (meta-data :origin plow :entry-date 20060615 :change-date nil :wn ("shot%1:06:01") :comments pq)
     (example "did the photographer get a shot of the volcano")
     )
    )
   )
))

