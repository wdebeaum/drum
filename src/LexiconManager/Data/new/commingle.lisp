;;;;
;;;; W::commingle
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::commingle
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("mix-22.1-1-1"))
     (LF-PARENT ONT::combine-objects)
     (TEMPL agent-affected2-optional-templ (xp (% w::pp (w::ptype w::with)))) ; like combine
     )
    )
   )
))

