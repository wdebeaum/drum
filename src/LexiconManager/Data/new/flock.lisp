;;;;
;;;; W::flock
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::flock
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("herd-47.5.2") :wn ("flock%2:38:00" "flock%2:38:01"))
     (LF-PARENT ONT::meet)
     (TEMPL AGENT-NP-PLURAL-TEMPL) ; like congregate,assemble,gather
     )
    )
   )
))

(define-words :pos W::n 
  :words (
   (W::flock
   (SENSES
    (
     (lf-parent ont::animal-group)
     (example "A flock of birds")
     (TEMPL pred-subcat-contents-templ)
     )
   ))
))
