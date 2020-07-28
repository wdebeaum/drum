;;;;
;;;; W::hesitate
;;;;

(define-words :pos W::v 
 :words (
  (W::hesitate
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("linger-53.1") :wn ("hesitate%2:42:00" "hesitate%2:42:01"))
     (LF-PARENT ONT::hesitate)
     ;(TEMPL agent-time-duration-templ) ; like pause
     (TEMPL experiencer-formal-XP-TEMPL (xp (% W::PP (W::ptype (? p W::about)))))
     )

    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("linger-53.1") :wn ("hesitate%2:42:00" "hesitate%2:42:01"))
     (LF-PARENT ONT::hesitate)
     ;(TEMPL agent-time-duration-templ) ; like pause
     (TEMPL experiencer-templ)
     )
    
    )
   )
))

