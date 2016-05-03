;;;;
;;;; W::spin
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::spin
    (wordfeats (W::morph (:forms (-vb) :past W::spun :nom w::spin)))
   (SENSES
    ((meta-data :origin "verbnet-2.0-corrected" :entry-date 20060315 :change-date nil :comments nil :vn ("roll-51.3.1") :wn ("spin%2:38:01" "spin%2:38:02"))
     (LF-PARENT ONT::rotate)
     (TEMPL affected-templ) ; like move,bounce but use more specific ont::rotate 
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::ROTATE)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin task-learning :entry-date 20050829 :change-date nil :comments nil :vn ("roll-51.3.1") :wn ("spin%2:38:01" "spin%2:38:02"))
     (example "spin the triangle to the left")
     )
    ((EXAMPLE "spin")
     (LF-PARENT ONT::rotate)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (templ agent-templ)
     (meta-data :origin task-learning :entry-date 20050829 :change-date nil :comments nil)
     )
    )
   )
))

