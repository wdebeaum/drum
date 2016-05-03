;;;;
;;;; W::whenever
;;;;

(define-words :pos W::ADV
 :words (
  (W::whenever
   (SENSES
    ((LF-PARENT ONT::event-time-rel)
     (TEMPL binary-constraint-S-decl-TEMPL)
     (example "I saw him whenever he passed by")
     )
    ((LF-PARENT ONT::event-time-rel)
     (meta-data :origin calo :entry-date 20040809 :change-date nil :comments caloy2)
     (example "buy a danish whenever buying a coffee")
     (TEMPL binary-constraint-s-ing-TEMPL)
     )
    )
   )
))

