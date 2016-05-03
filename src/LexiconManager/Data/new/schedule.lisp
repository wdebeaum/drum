;;;;
;;;; W::schedule
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::schedule
   (wordfeats (W::morph (:forms (-vb) :nom W::schedule)))
   (SENSES
    ((LF-PARENT ONT::planning)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-EFFECT-OBJCONTROL-TEMPL)
     (example "schedule the meeting to start at 10 pm" "schedule the action for 10pm")
     (meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090515 :comments nil :vn ("create-26.4-1"))
     )
    )
   )
))

