;;;;
;;;; W::DISTANCE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::DISTANCE
   (SENSES
    ((meta-data :origin plow :entry-date 20060803 :change-date nil :comments nil :wn ("distance%1:07:00"))
     (LF-PARENT ONT::DISTANCE)
     (example "the distance to the hotel")
     (TEMPL OTHER-RELN-neutral-TEMPL (xp (% W::pp (W::ptype (? ptp W::to W::from)))))
    ;; (templ other-reln-templ)
     )
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("distance%1:07:00"))
     (LF-PARENT ONT::DISTANCE)
     (example "a distance of five miles")
     (TEMPL reln-subcat-of-units-TEMPL)
     )
    )
   )
))

