;;;;
;;;; W::absolve
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::absolve
   (SENSES
    ((meta-data :origin "verbnet-1.5-corrected" :entry-date 20051219 :change-date 20090508 :comments nil :vn ("cheat-10.6-1-1"))
     (LF-PARENT ONT::pardon)
     (TEMPL AGENT-AFFECTED-THEME-OPTIONAL-TEMPL (xp (% w::pp (w::ptype w::of)))) ; like pardon but different template
     )
    )
   )
))

