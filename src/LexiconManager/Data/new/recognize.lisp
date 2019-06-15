;;;;
;;;; W::recognize
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::recognize
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("conjecture-29.5-1"))
     (LF-PARENT ONT::come-to-understand)
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% w::cp (w::ctype w::s-finite)))) ; like realize
     (example "he recognized that he was a spy")
     (PREFERENCE 0.98)
     )
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (LF-PARENT ONT::come-to-understand)
     (example "He recognized him [as a spy)")
     (TEMPL AGENT-NEUTRAL-FORMAL-2-XP-3-XP2-PP-OPTIONAL-TEMPL )
     )
    )
   )
))

