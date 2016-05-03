;;;;
;;;; W::proxy
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::proxy
   (SENSES
    ((LF-PARENT ONT::replacement)
     (templ other-reln-theme-templ  (xp (% W::pp (W::ptype (? pt W::for)))))
     (EXAMPLE "connect through a proxy server")
     (meta-data :origin task-learning :entry-date 20050815 :change-date nil :wn ("proxy%1:18:00") :comments nil)
     )
    )
   )
))

