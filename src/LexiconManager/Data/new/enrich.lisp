;;;;
;;;; W::enrich
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(W::enrich
   (SENSES
    ((example "this feature enriches the experience")
     (sem (f::aspect f::dynamic))
     (meta-data :origin cardiac :entry-date 20090406 :change-date nil :comments nil)
     (LF-PARENT ONT::improve)
     (templ agent-affected-xp-templ)
     )
    )
   )
))

