;;;;
;;;; W::TILL
;;;;

(define-words :pos W::ADV
 :words (
  (W::TILL
   (SENSES
    ((LF-PARENT ONT::event-time-rel)
     (example "wait till he leaves")
     (LF-FORM W::UNTIL)
     (TEMPL binary-constraint-S-decl-TEMPL)
     )
    ((LF-PARENT ONT::event-time-rel)
     (example "wait till 5")
     (LF-FORM W::UNTIL)
     (TEMPL binary-constraint-S-TEMPL)
     )
    )
   )
))

