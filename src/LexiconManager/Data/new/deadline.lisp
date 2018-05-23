;;;;
;;;; W::DEADLINE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::DEADLINE
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("deadline%1:28:00"))
     (LF-PARENT ONT::time-defined-by-event)
     (templ other-reln-templ (xp (% W::pp (W::ptype (? pt w::for w::of)))))
     )
    )
   )
))

