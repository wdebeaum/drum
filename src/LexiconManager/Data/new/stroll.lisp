;;;;
;;;; W::stroll
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::stroll
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("stroll%2:38:00" "stroll%2:38:01"))
     (LF-PARENT ONT::SELF-LOCOMOTE)
     (TEMPL AGENT-TEMPL)
     (example "walk to avon")
     )
    ((meta-data :origin general :entry-date 20110127 :change-date nil :comments jansen :vn ("run-51.3.2") :wn ("travel%2:38:00" "travel%2:38:02"))
     (LF-PARENT ONT::self-locomote)
     (example "he strolled the path")
     (TEMPL agent-neutral-templ) ; like stroll,walk
     (PREFERENCE 0.96)
     )
    )
   )
))

