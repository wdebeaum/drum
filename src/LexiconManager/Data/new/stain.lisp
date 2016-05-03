;;;;
;;;; W::stain
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::stain
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("coloring-24") :wn ("stain%2:30:00" "stain%2:30:01" "stain%2:30:04"))
     (LF-PARENT ont::coloring)
 ; like color
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("coloring-24") :wn ("stain%2:30:00" "stain%2:30:01" "stain%2:30:04"))
     (LF-PARENT ont::coloring)
     (TEMPL agent-affected-result-optional-templ (xp (% w::adjp (w::set-modifier -)))) ; like color,paint
     )
    )
   )
))

