;;;;
;;;; W::obsess
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::obsess
    (wordfeats (W::morph (:forms (-vb) :nom W::obsession)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090511 :comments nil :vn ("amuse-31.1") :wn ("obsess%2:37:00"))
     (LF-PARENT ONT::care)
     (TEMPL agent-theme-xp-templ (xp (% w::pp (w::ptype (? p w::over w::about))))) ; like mind,worry
     )
    )
   )
))

