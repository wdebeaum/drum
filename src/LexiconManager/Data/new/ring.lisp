;;;;
;;;; W::ring
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::ring
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("contiguous_location-47.8") :wn ("ring%2:35:03"))
     (LF-PARENT ONT::surround)
     (TEMPL neutral-neutral-xp-templ) ; like cover,surround
     )
    ((meta-data :origin cause-result-relations :entry-date 20091216 :change-date nil :comments wf-missing-mappings)
     (LF-PARENT ONT::make-sound)
     (example "the bell rang")
     (TEMPL agent-templ) ;; agent (bell) emits a sound        
     )
    ((meta-data :origin cause-result-relations :entry-date 20091216 :change-date nil :comments wf-missing-mappings)
     ;(LF-PARENT ONT::play)
     (LF-PARENT ONT::make-sound)
     (example "Joe rang the bell")
     (TEMPL agent-affected-xp-templ)
     )     
    )
   )
))

