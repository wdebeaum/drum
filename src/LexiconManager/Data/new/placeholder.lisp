;;;;
;;;; W::placeholder
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::placeholder
   (SENSES
    ((LF-PARENT ONT::location)
     (templ other-reln-theme-templ (xp (% W::pp (W::ptype (? pt W::for)))))
     (EXAMPLE "add a placeholder box")
     (meta-data :origin task-learning :entry-date 20050815 :change-date nil :comments nil)
     )
    )
   )
))

