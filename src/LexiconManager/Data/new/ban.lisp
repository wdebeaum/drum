;;;;
;;;; W::ban
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::ban
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090528 :comments nil :vn ("forbid-67"))
     (LF-PARENT ONT::prohibit)
     (TEMPL agent-effect-xp-templ)
     (example "The hospital banned smoking")
     )
    ((meta-data :origin "gloss-training" :entry-date 20100217 :change-date nil :comments nil)
     (LF-PARENT ONT::prohibit)
     (TEMPL AGENT-EFFECT-AFFECTED-OBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-from-ing) (w::ptype w::from))))
     (example "It bans him from doing something")
     )
    )
   )
))

