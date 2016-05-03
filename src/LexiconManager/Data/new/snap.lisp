;;;;
;;;; W::snap
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::snap
   (SENSES
    #||((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("break-45.1") :wn ("snap%2:30:00" "snap%2:35:01"))
     (LF-PARENT ont::break-object)
 ; like tear
     )||#
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("break-45.1") :wn ("snap%2:30:00" "snap%2:35:01"))
     (LF-PARENT ont::break-object)
     (TEMPL agent-affected-xp-templ) ; like break
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("break-45.1") :wn ("snap%2:30:00" "snap%2:35:01"))
     (LF-PARENT ont::break-object)
     (TEMPL affected-templ) ; like crash,tear,break
     )
    
    )
   )
))

