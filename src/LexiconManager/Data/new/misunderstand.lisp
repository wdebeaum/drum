;;;;
;;;; W::misunderstand
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::misunderstand
   (wordfeats (W::morph (:forms (-vb) :past W::misunderstood)))
   (SENSES
    ((LF-PARENT ONT::Misunderstand)
     (meta-data :origin "trips" :entry-date 20060315 :change-date nil :comments nil :wn ("misunderstand%2:31:01"))
     (example "I misunderstood the situation")
     (SEM (F::Aspect F::stage-level))
     (TEMPL agent-theme-xp-templ)
     )
    )
   )
))

