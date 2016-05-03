;;;;
;;;; W::galvanize
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::galvanize
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("amuse-31.1") :wn ("galvanize%2:37:00"))
     (LF-PARENT ont::provoke)
     (TEMPL agent-affected-theme-objcontrol-optional-templ)
     (example "the attraction of Napoleon was enough to galvanize him [to become the Emporer's painter]")
     )
    ((lf-parent ont::provoke)
     (example "the news galvanized him into action")
     (TEMPL agent-affected-effect-templ (xp (% w::pp (w::ptype (? pt w::into w::to))))) ; like annoy,bother,concern,hurt
    )
    )
   )
))

