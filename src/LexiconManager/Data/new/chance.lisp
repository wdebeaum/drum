;;;;
;;;; W::chance
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::chance
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("chance%1:07:00"))
     (LF-PARENT ONT::likely-scale)
      (TEMPL OTHER-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  (W::chance
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("occurrence-48.3"))
     (LF-PARENT ONT::happen)
     (example "he chanced to see it")
     
     (TEMPL EXPERIENCER-FORMAL-SUBJCONTROL-TEMPL  (xp (% W::cp (W::ctype W::s-to))))
     (SYNTAX (w::exclude-passive +))
     )
    )
   )
))

