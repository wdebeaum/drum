;;;;
;;;; W::use
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
  (W::use
     (wordfeats (W::morph (:forms (-vb) :nom W::use)))
   (SENSES
    ((LF-PARENT ONT::USE)
     (example "use your head")
     (SEM (F::ASPECT F::DYNAMIC))
     )
    ((LF-Parent ont::use)
     (example "a battery uses a chemical reaction to maintain voltage" "the engine uses gasoline")
     (meta-data :origin (beetle2 mobius) :entry-date 20080218 :change-date nil :comments pilot1)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

