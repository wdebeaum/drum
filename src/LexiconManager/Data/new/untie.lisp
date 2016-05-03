;;;;
;;;; W::untie
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::untie
   (wordfeats (W::morph (:forms (-vb) :past W::untied :ing W::untying)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("disassemble-23.3") :wn ("untie%2:35:00"))
     (LF-PARENT ONT::unattach)
     (TEMPL agent-affected-theme-optional-templ (xp (% w::pp (w::ptype w::from)))) ; like disconnect
     )
    )
   )
))

