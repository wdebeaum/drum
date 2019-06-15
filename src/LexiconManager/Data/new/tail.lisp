;;;;
;;;; W::tail
;;;;

(define-words :pos W::n :templ count-pred-templ
 :tags (:base500)
 :words (
(W::tail
   (SENSES
    ((meta-data :origin bolt :entry-date 20120516 :comments top500)
     (LF-PARENT ONT::object-dependent-location)
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :tags (:base500)
 :words (
  (W::tail
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("chase-51.6") :wn ("tail%2:38:00"))
     (LF-PARENT ONT::pursue)
 ; like track
     )
    )
   )
))

