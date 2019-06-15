;;;;
;;;; W::weigh
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::weigh
   (SENSES
    (;(LF-PARENT ONT::weigh)
     (LF-PARENT ONT::MEASURE)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL neutral-extent-xp-templ)
     (example "the truck weighs five lbs")
     (meta-data :origin calo :entry-date 20041206 :change-date nil :comments caloy2 :vn ("register-54.1") :wn ("weigh%2:42:00" "weigh%2:42:01"))
     )
    ((LF-PARENT ONT::weigh)
     (SEM (F::Aspect F::dynamic) (F::Time-span F::extended))
     (TEMPL AGENT-neutral-XP-TEMPL)
     (example "he weighed the truck")
     (preference .94) 
     (meta-data :origin calo :entry-date 20041206 :change-date nil :comments caloy2 :vn ("register-54.1") :wn ("weigh%2:42:00" "weigh%2:42:01"))
     )
    )
   )
))

