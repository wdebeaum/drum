;;;;
;;;; W::subscription
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::subscription
   (SENSES
    ((LF-PARENT ONT::order)
     (TEMPL other-reln-templ (xp (% W::pp (W::ptype (? pt W::to)))))
     (EXAMPLE "which subscriptions have new content")
     (meta-data :origin task-learning :entry-date 20050815 :change-date nil :comments nil)
     )
    )
   )
))

