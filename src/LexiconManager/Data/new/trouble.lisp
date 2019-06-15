;;;;
;;;; W::TROUBLE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::TROUBLE
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("trouble%1:04:02"))
     (LF-PARENT ONT::trouble)
     (TEMPL mass-PRED-TEMPL)
     )
    ((LF-PARENT ONT::trouble)
     (example "he has trouble doing that")
     (meta-data :origin calo :entry-date 20031130 :change-date nil :wn ("trouble%1:04:02") :comments caloy2)
     (TEMPL mass-PRED-assoc-with-TEMPL  (XP (% W::vp (W::vform W::ing))))
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::trouble
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("trouble%2:37:00" "trouble%2:37:01"))
     (LF-PARENT ONT::evoke-bother)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

