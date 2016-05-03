;;;;
;;;; W::advise
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::advise
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("advise-37.9-1"))
     (LF-PARENT ONT::advise)
     (TEMPL agent-addressee-theme-optional-templ) ; like warn
     (PREFERENCE 0.96)
     )
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (LF-PARENT ONT::advise)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "I'll advise him to do it")
     (TEMPL agent-addressee-theme-objcontrol-req-templ (xp (% W::cp (W::ctype W::s-to))))
     )
    )
   )
))

