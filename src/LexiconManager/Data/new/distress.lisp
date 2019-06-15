;;;;
;;;; W::distress
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::distress
   (SENSES
    ((LF-PARENT ONT::trouble)
     (meta-data :origin calo :entry-date 20090403 :change-date nil :comments nil)
     (templ mass-pred-templ)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (w::distress
  (senses
   ((meta-data :wn ("distress%1:12:02"))
    (LF-PARENT ONT::feeling)
    (TEMPL mass-pred-TEMPL)
    (syntax (W::morph (:forms (-none))))
    )
   )
)
))

(define-words :pos W::n
 :words (
  (w::distress
  (senses
   ((meta-data :wn ("distress%1:26:00"))
    (LF-PARENT ONT::distress-scale)
    (TEMPL mass-pred-TEMPL)
    (syntax (W::morph (:forms (-none))))
    )
   )
)
))

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::distress
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("distress%2:37:00"))
     (LF-PARENT ONT::evoke-distress)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

