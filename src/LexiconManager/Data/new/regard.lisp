;;;;
;;;; W::regard
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::regard
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("characterize-29.2") :wn ("regard%2:31:00"))
     (LF-PARENT ONT::belief-ascription)
     (TEMPL EXPERIENCER-NEUTRAL-FORMAL-2-XP-3-XP2-PP-TEMPL) 
     )
    ;; this sense is needed so the passive-by rule can apply 
    ((LF-PARENT ONT::scrutiny)
     (example "he regarded his opponent warily")
     (TEMPL agent-neutral-xp-templ)
     (meta-data :origin calo :entry-date 20060124 :change-date nil :comments meeting-understanding :vn ("sight-30.2") :wn ("regard%2:39:00"))
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::regard
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("sight-30.2") :wn ("regard%2:39:00"))
     (LF-PARENT ONT::active-perception)
     (TEMPL agent-neutral-templ) ; like observe,view,watch
     )
    )
   )
))

