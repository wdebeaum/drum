;;;;
;;;; W::conjoin
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
   (W::conjoin
    (SENSES
      ((meta-data :origin "verbnet-1.5-corrected" entry-date 20060214 :change-date nil :comments nil :vn ("amalgamate-22.2-2") )
       (LF-PARENT ONT::combine-objects)
       (TEMPL agent-affected2-templ (xp (% w::pp (w::ptype w::with)))) ; like associate
       )
     )
    )
))

