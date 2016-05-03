;;;;
;;;; W::startle
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::startle
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("amuse-31.1") :wn ("startle%2:37:00"))
     (LF-PARENT ont::provoke)
     (example "the thunder startled him [into action]")
     (TEMPL agent-affected-effect-optional-templ (xp (% w::pp (w::ptype (? pt w::into))))) ; like annoy,bother,concern,hurt
     )
    )
   )
))

