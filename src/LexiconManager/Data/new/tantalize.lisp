;;;;
;;;; W::tantalize
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::tantalize
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("amuse-31.1") :wn ("tantalize%2:32:00"))
     (LF-PARENT ont::provoke)
     (TEMPL agent-affected-effect-optional-templ (xp (% w::pp (w::ptype (? pt w::into))))) ; like annoy,bother,concern,hurt    
     )
    )
   )
))

