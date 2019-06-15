;;;;
;;;; W::wrap
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::wrap
   (SENSES
    ((LF-PARENT ONT::arrange-text)
     (meta-data :origin task-learning :entry-date 20050926 :change-date 20090709 :comments nil)
     (example "make text wrap around an object")
     )
    ((LF-PARENT ONT::surround)
     ;(SEM (F::Aspect F::bounded))
     (meta-data :origin task-learning :entry-date 20050926 :change-date 20090709 :comments nil)
     (example "he wrapped his car around a tree")
     )
    ((LF-PARENT ONT::cause-cover)
     ;(TEMPL agent-affected-xp-templ (xp (% W::PP (W::ptype (? pt W::with W::in)))))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (meta-data :origin trips :entry-date 20090911 :change-date nil :comments nil)
     (example "wrap the present")
     )
    
    )
   )
))

