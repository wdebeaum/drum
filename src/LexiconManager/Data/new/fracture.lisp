;;;;
;;;; W::fracture
;;;;

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::fracture
   (SENSES
    #||((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("break-45.1") :wn ("fracture%2:29:00" "fracture%2:29:01" "fracture%2:29:02" "fracture%2:30:10"))
     (LF-PARENT ont::break-object)
 ; like tear
     )||#
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("break-45.1") :wn ("fracture%2:29:00" "fracture%2:29:01" "fracture%2:29:02" "fracture%2:30:10"))
     (LF-PARENT ont::break-object)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) ; like break
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("break-45.1") :wn ("fracture%2:29:00" "fracture%2:29:01" "fracture%2:29:02" "fracture%2:30:10"))
     (LF-PARENT ont::break-object)
     (TEMPL affected-templ) ; like crash,tear,break
     )
    
   )
)))

