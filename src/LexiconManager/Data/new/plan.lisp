;;;;
;;;; W::PLAN
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::PLAN
   (SENSES
    #|
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("plan%1:09:00"))
     (EXAMPLE "let's make a plan")
     (LF-PARENT ONT::procedure)
     )
    |#
    ((meta-data :origin domain-reorganization :entry-date 20170904 :change-date nil :comments nil :wn ("plan%1:09:01"))
     (EXAMPLE "plans to take over the world")
     (LF-PARENT ONT::mental-plan)
     )
    ; nom
;    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("plan%1:09:00"))
;     (EXAMPLE "Cancel this plan -- meaning cancel the actions connected with the plan")
;     (LF-PARENT ONT::ACTION)
;     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::PLAN
   (wordfeats (W::morph (:forms (-vb) :past W::planned :ing w::planning)))
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090515 :comments nil :vn ("wish-62") :wn ("plan%2:31:01" "plan%2:31:00" "plan%2:36:00"))
     (LF-PARENT ONT::planning)
     (templ agent-effect-objcontrol-templ)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (EXAMPLE "Let's plan the evacuation [to go out this door]")
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090515 :comments nil :vn ("wish-62")  :wn ("plan%2:31:01" "plan%2:31:00" "plan%2:36:00"))
     (LF-PARENT ONT::planning)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-theme-xp-templ (xp (% W::cp (W::ctype W::s-to))))
     (EXAMPLE "Let's plan to evacuate people")
     )
    )
   )
))

