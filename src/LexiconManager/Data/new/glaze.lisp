;;;;
;;;; w::glaze
;;;;

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
(w::glaze
 (senses
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("coloring-24") :wn ("glaze%2:35:00" "glaze%2:36:00"))
     (LF-PARENT ont::coloring)
     (TEMPL agent-affected-result-optional-templ (xp (% w::adjp (w::set-modifier -)))) ; like lacquer,paint,varnish,tint,spraypaint,dye,color,stain,shellac,distemper,enamel
     (PREFERENCE 0.96)
     )
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil)
   (LF-PARENT ONT::coat-food)
   (example "glaze the chicken")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   )
  )
 )
))

