;;;;
;;;; W::moon
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::moon
   (SENSES
    ((meta-data :origin integrated-learning :entry-date 20050817 :change-date nil :comments nil :wn ("moon%1:17:01" "moon%1:17:00"))
     (LF-PARENT ONT::natural-object)
     (example "the phases of the moon")
     )
    )
   )
))

(define-words :pos W::V :templ agent-theme-xp-templ
 :tags (:base500)
 :words (
  (W::moon
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090511 :comments nil :vn ("marvel-31.3-2"))
     (LF-PARENT ONT::regretting)
     (TEMPL agent-theme-xp-templ (xp (% w::pp (w::ptype (? p w::about))))) ; like mind,worry
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090511 :comments nil :vn ("marvel-31.3-2"))
     (LF-PARENT ONT::regretting)
     (TEMPL agent-templ) ; like mind
     )
    )
   )
))

