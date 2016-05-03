;;;;
;;;; W::instruct
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::instruct
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("advise-37.9-1"))
     (LF-PARENT ONT::command)
     (TEMPL agent-addressee-theme-optional-templ) ; like warn
     (PREFERENCE 0.96)
     )
    ((meta-data :origin task-learning :entry-date 20050825 :change-date nil :comments nil)
     (LF-PARENT ONT::command)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "I'll instruct him to do it")
     (TEMPL agent-addressee-theme-objcontrol-req-templ) 
     )
    )
   )
))

