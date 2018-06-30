;;;;
;;;; W::VALUE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::VALUE
   (SENSES
    ((LF-PARENT ONT::value-cost) 
     (example "He gave it a value of $10")
     (TEMPL reln-subcat-of-units-TEMPL)
     )
    ((LF-PARENT ONT::value-COST)
     (TEMPL other-reln-templ (xp (% W::pp (W::ptype w::of))))
     (example "What is the value of my car?")
     )
    
    ((LF-PARENT ONT::value)
     (TEMPL other-reln-templ)
     (example "what is the value of the tempertaure variable?.  Take the value of x")
     ))
   ))
 )

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::value
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090522 :comments nil :vn ("price-54.4") :wn ("value%2:31:00" "value%2:31:02" "value%2:31:03"))
     (LF-PARENT ONT::scrutiny)
     (templ agent-neutral-xp-templ)
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("admire-31.2") :wn ("value%2:31:01" "value%2:40:00"))
     (LF-PARENT ONT::appreciate)
     (TEMPL experiencer-neutral-xp-templ) ; like admire,adore,appreciate,despise,detest,dislike,loathe,miss
     )
    )
   )
))

