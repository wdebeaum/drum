;;;;
;;;; W::fret
;;;;

(define-words :pos W::V 
 :words (
;   )
  (W::fret
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090511 :comments nil :vn ("marvel-31.3-2"))
     (LF-PARENT ONT::state-of-worrying)
     (TEMPL experiencer-formal-xp-templ (xp (% w::pp (w::ptype (? p w::about))))) ; like mind,worry
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090511 :comments nil :vn ("marvel-31.3-2"))
     (LF-PARENT ONT::state-of-worrying)
     (TEMPL experiencer-templ) ; like mind
     )
    )
   )
))

