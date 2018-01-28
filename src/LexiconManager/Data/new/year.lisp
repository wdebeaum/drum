;;;;
;;;; W::YEAR
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::YEAR
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::year-duration)
     (example "a 3 year warranty")
     (templ substance-unit-templ)
     )
    ((meta-data :origin plow :entry-date 20080117 :change-date nil :comments nil :wn ("month%1:28:00"))
     (LF-PARENT ONT::time-interval)
     (templ time-reln-templ)
     )
    ((LF-PARENT ONT::time-interval)
     (templ other-reln-templ (xp (% W::pp (W::ptype (? pt w::for w::of)))))
     )
    )
   )
))

