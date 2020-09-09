;;;;
;;;; W::motivation
;;;;
#|  ;; now nominalization of motivate
(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::motivation
   (SENSES
    ((LF-PARENT ONT::motive)
     (example "what is the motivation for this proposal")
     (TEMPL OTHER-RELN-TEMPL (xp (% W::pp (W::ptype (? pt W::for W::of)))))
     (meta-data :origin integrated-learning :entry-date 20050817 :change-date nil :wn ("motivation%1:03:00") :comments nil)
     )
    )
   )
))

|#
