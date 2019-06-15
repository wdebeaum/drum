;;;;
;;;; W::cram
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::cram
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-1-1"))
     (LF-PARENT ONT::fill-container)
     (TEMPL AGENT-AFFECTEDR-AFFECTED-XP-PP-WITH-2-OPTIONAL-TEMPL (xp (% w::pp (w::ptype (? t w::with))))) ; like crowd,jam,pack,pile
     (PREFERENCE 0.96)
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("spray-9.7-1-1"))
     (LF-PARENT ONT::Fill-container)
     (example "cram the oranges in/into/on the truck")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-RESULT-ADVBL-OBJCONTROL-TEMPL)
     )
    )
   )
))

