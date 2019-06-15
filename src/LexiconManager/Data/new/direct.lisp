;;;;
;;;; w::direct
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (w::direct
   (wordfeats (W::morph (:forms (-vb) :nom w::direction)))
   (senses
    #|
    ((meta-data :origin coordops :entry-date 20070511 :change-date nil :comments nil :vn ("judgement-33"))
     (LF-PARENT ont::guiding)
     (example "team alpha will direct the activity")
     (TEMPL agent-affected-xp-templ)
     )
    |#
    ((meta-data :origin coordops :entry-date 20070511 :change-date nil :comments nil :vn ("judgement-33"))
     (LF-PARENT ont::guiding)
     ;(LF-PARENT ont::manage)
     (example "team alpha will direct")
     (TEMPL agent-templ)
     )

    (;(LF-PARENT ONT::control-manage)
     ;(LF-PARENT ont::manage)
     (LF-PARENT ont::guiding)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "the supervisor directs the crew")
     )

    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::DIRECT
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("direct%3:00:00"))
     (LF-PARENT ONT::ROUTE-TOPOLOGY-val)
     (example "a direct route")
     (SEM (F::GRADABILITY F::+))
     )
    ;; a direct comment?
    )
   )
))

