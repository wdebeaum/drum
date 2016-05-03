;;;;
;;;; W::immerse
;;;;

(define-words :pos W::v :templ agent-AFFECTED-xp-templ
 :words (
  (W::immerse
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("put-9.1") :wn ("immerse%2:35:00"))
     (LF-PARENT ONT::immerse)
 ; like insert,position
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("put-9.1") :wn ("immerse%2:35:00"))
     (LF-PARENT ONT::immerse)
     (TEMPL AGENT-AFFECTED-GOAL-TEMPL (xp  (% W::ADVBL (W::lf (% ?p (w::class (? x ont::in-loc)))))))
     ))
    
   
   )
))

