;;;;
;;;; W::mingle
;;;;

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::mingle
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("mix-22.1-1-1"))
     (LF-PARENT ONT::combine-objects)
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-OPTIONAL-TEMPL (xp (% w::pp (w::ptype w::with)))) ; like combine
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("mix-22.1-1-1"))
     (LF-PARENT ONT::combine-objects) ; like mix
     )
    )
   )
))

