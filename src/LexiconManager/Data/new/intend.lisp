;;;;
;;;; W::intend
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::intend
   (wordfeats (W::morph (:forms (-vb) :nom w::intention)))
   (SENSES
    ((LF-PARENT ONT::Intention)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL experiencer-theme-xp-templ (xp (% W::cp (W::ctype W::s-to))))
     (EXAMPLE "remove the addresses if you don't intend to use them")
     (meta-data :origin task-learning :entry-date 20050916 :change-date nil :comments nil :vn ("wish-62") :wn ("intend%2:31:00" "intend%2:31:01"))
     )
    ((LF-PARENT ONT::Intention)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL EXPERIENCER-NEUTRAL-FORMAL-CP-OBJCONTROL-B-TEMPL)
     (EXAMPLE "intend something to move towards a certain goal")
     (meta-data :origin jr :entry-date 20120806 :change-date nil :comments gloss-owl)
     )

   ((LF-PARENT ONT::ENCODES-MESSAGE)
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     (EXAMPLE "an example sentence would show what this word intended")
     )
      
    )
   )
))

