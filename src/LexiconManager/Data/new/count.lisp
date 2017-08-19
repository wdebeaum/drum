;;;;
;;;; W::count
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::count
   (SENSES
    ((LF-PARENT ont::rely)
     (TEMPL agent-neutral-XP-TEMPL (xp (% W::PP (W::ptype W::on))))
     (example "a product you can count on")
     (meta-data :origin calo :entry-date 20041122 :change-date nil :comments caloy2)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::count
   (SENSES
    ((meta-data :origin domain-reorganization :entry-date 20060803 :change-date nil :comments nil :wn ("count%1:23:00"))
     (LF-PARENT ONT::total-scale)
     )
    )
   )
))

