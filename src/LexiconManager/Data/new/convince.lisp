;;;;
;;;; W::convince
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::convince
   (SENSES
    ((meta-data :origin "wordnet-3.0" :entry-date 20090602 :change-date nil :comments nil)
     (LF-PARENT ONT::convince)
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-OPTIONAL-TEMPL (xp (% W::cp (W::ctype (? ct W::s-finite w::s-to)))))
     (example "convince me that I should stay")
     )
    ((meta-data :origin "wordnet-3.0" :entry-date 20090602 :change-date nil :comments nil)
     (LF-PARENT ONT::convince)
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-OPTIONAL-TEMPL (xp (% W::pp (W::ptype W::of))))
     (example "convince me of its value")
     )
    ((meta-data :origin "wordnet-3.0" :entry-date 20090602 :change-date nil :comments nil)
     (LF-PARENT ONT::convince)
     (TEMPL AGENT-AGENT1-FORMAL-OBJCONTROL-TEMPL)
     (example "he convinced me to stay")
     )
    )
   )
))

