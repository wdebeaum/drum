;;;;
;;;; W::evacuate
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::evacuate
   (wordfeats (W::morph (:forms (-vb) :nom w::evacuation)))
   (SENSES
#|
    ((LF-PARENT ONT::evacuate)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "evacuate delta")
     )
|#
    ; a little funny to have both the people and the geographic location to be AFFECTED
    (;(LF-PARENT ONT::empty)
     (LF-PARENT ONT::cause-clear)
     (example "evacuate the building (of people)")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTEDR-AFFECTED-XP-PP-OF-OPTIONAL-TEMPL)
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090529 :comments nil :vn ("banish-10.2") :wn ("evacuate%2:38:00" "evacuate%2:38:01"))
     ;(LF-PARENT ONT::empty)
     (LF-PARENT ONT::cause-clear)
     (example "evacuate the people" "evacuate the people from the burning building")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL)
     )
    (
     ;(LF-PARENT ONT::empty)
     (LF-PARENT ONT::cause-clear)
     (example "The people evacuated")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-TEMPL)
     )
    
    )
   )
))

