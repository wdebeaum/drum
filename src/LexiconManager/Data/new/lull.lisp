;;;;
;;;; W::lull
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::lull
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("amuse-31.1") :wn ("lull%2:37:01"))
     (LF-PARENT ont::provoke)
     (TEMPL agent-affected-effect-optional-templ (xp (% w::pp (w::ptype (? pt w::into w::to))))) ; like annoy,bother,concern,hurt
     )
    )
   )
))

