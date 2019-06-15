;;;;
;;;; W::conjoin
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (W::conjoin
    (SENSES
      ((meta-data :origin "verbnet-1.5-corrected" entry-date 20060214 :change-date nil :comments nil :vn ("amalgamate-22.2-2") )
       (LF-PARENT ONT::combine-objects)
       (TEMPL AGENT-AFFECTED-AFFECTED1-XP-TEMPL (xp (% w::pp (w::ptype w::with)))) ; like associate
       )
     )
    )
))

