
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::FOLLOWING
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("following%5:00:02:succeeding(a):00"))
     (lf-parent ont::sequence-val-next)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  ((W::following)
   (SENSES
    ((LF-PARENT ONT::event-time-rel)
     (TEMPL binary-constraint-S-OR-NP-templ)  
     (example "following the meeting she checked her watch")
     )
    )
   )
))
