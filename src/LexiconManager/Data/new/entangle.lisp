;;;;
;;;; W::entangle
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::entangle
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("amalgamate-22.2-2"))
     (LF-PARENT ONT::associate)
     (TEMPL agent-affected2-optional-templ (xp (% w::pp (w::ptype w::with)))) ; like associate,associate
     )
    )
   )
))

