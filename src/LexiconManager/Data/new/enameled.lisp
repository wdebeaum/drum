;;;;
;;;; W::enameled
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::enameled
   (wordfeats (W::morph (:forms NIL)) (W::vform (? vf W::past w::pastpart)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("coloring-24"))
     (LF-PARENT ont::coloring)
 ; like color
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("coloring-24"))
     (LF-PARENT ont::coloring)
     (TEMPL agent-affected-result-optional-templ (xp (% w::adjp (w::set-modifier -)))) ; like color,paint
     )
    )
   )
))

