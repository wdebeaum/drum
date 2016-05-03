;;;;
;;;; W::reschedule
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
   (W::reschedule
   (wordfeats (W::morph (:forms (-vb) :ing W::rescheduling)))
   (SENSES
    ((LF-PARENT ONT::planning)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-EFFECT-OBJCONTROL-TEMPL)
     (example "reschedule the meeting to start at 10 pm" "reschedule the action for 10pm")
     (meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090515 :comments nil :vn ("create-26.4-1"))
     )
    )
   )
))

