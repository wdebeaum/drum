;;;;
;;;; W::collect
;;;;

(define-words :pos W::v 
 :words (
  (W::collect
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("herd-47.5.2") :wn ("collect%2:35:00" "collect%2:35:01" "collect%2:38:00" "collect%2:40:00"))
     (LF-PARENT ONT::meet)
     (example "the crowd collected in the square")
     (TEMPL agent-plural-templ) ; like congregate,assemble,gather
     (PREFERENCE 0.96)
     )
    
    ((meta-data :origin "wordnet-3.0" :entry-date 20090430 :change-date nil :comments nil)
     (LF-PARENT ONT::collect)
     
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

