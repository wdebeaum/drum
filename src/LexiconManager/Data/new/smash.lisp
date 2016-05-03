;;;;
;;;; w::smash
;;;;

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
(w::smash
 (senses
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("break-45.1") :wn ("smash%2:30:00" "smash%2:35:08"))
     (LF-PARENT ont::crush)
     (TEMPL agent-affected-xp-templ) ; like break
     (PREFERENCE 0.96)
     )
    
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("break-45.1") :wn ("smash%2:30:00" "smash%2:35:08"))
     (LF-PARENT ont::crush)
     (example "it smashed against the window")
     (TEMPL affected-templ) ; like crash,tear,break
     (PREFERENCE 0.96)
     )
  ))))

