;;;;
;;;; W::SHAME
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::SHAME
   (SENSES
    ((LF-PARENT ONT::trouble)
     (example "that's a shame")
     (meta-data :origin calo :entry-date 20050527 :change-date nil :wn ("shame%1:11:00") :comments projector-purchasing)
     (templ bare-pred-templ)
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::shame
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("shame%2:37:00" "shame%2:41:00"))
     (LF-PARENT ONT::evoke-shame)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

