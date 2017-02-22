;;;;
;;;; W::stack
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::stack
   (SENSES
    #||((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-2"))
     ;(LF-PARENT ONT::fill-container)
     (LF-PARENT ONT::FILL-CONTAINER)
     (TEMPL agent-goal-affected-templ (xp (% w::pp (w::ptype (? t w::with))))) ; like load
     )||#
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-2"))
     ;(LF-PARENT ONT::fill-container)
     (LF-PARENT ONT::ARRANGING)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

(define-words :pos W::n 
 :words (
  (W::stack
   (SENSES
    (
     (LF-PARENT ONT::column-formation)
;     (TEMPL classifier-count-pl-templ)
     (TEMPL other-reln-templ)
     (EXAMPLE "A row of ducks")
     )))
  ))
