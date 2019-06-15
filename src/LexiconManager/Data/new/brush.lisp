;;;;
;;;; W::brush
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
((W::brush (w::up))
   (SENSES
    ((example "he wants to brush up on his french")
     (sem (f::aspect f::dynamic))
     (meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
     (LF-PARENT ONT::improve)
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::pp (W::ptype W::on))))
     )
     ((example "he wants to brush up his french")
     (sem (f::aspect f::dynamic))
     (meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
     (LF-PARENT ONT::improve)
     (TEMPL AGENT-FORMAL-XP-TEMPL )
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::brush
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-1"))
     (LF-PARENT ONT::cause-move)
 ; like spray
     )
    )
   )
))

