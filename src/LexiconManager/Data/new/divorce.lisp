;;;;
;;;; w::divorce
;;;;

(define-words :pos W::n
 :words (
  (w::divorce
  (senses
   ((LF-PARENT ONT::social-event)
    (meta-data :origin chf :entry-date 20070809 :change-date nil :comments nil)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::divorce
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("separate-23.1-1"))
     (LF-PARENT ONT::separation)
 ; like divide
     )
    )
   )
))

