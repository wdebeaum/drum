;;;;
;;;; W::PAINT
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
 (W::PAINT
   (SENSES
    ((meta-data :origin fruit-carts :entry-date 20041103 :change-date nil :comments nil)
     (lf-parent ont::coloring)
     (SEM (F::aspect F::bounded) (F::time-span F::atomic))
     (example "paint the triangle")
     )
    ((meta-data :origin fruit-carts :entry-date 20041103 :change-date nil :comments nil)
     (lf-parent ont::coloring)
     (SEM (F::aspect F::bounded) (F::time-span F::atomic))
     (example "paint the triangle red")
     (TEMPL agent-affected-result-optional-templ (xp (% w::ADJP  (w::set-modifier -))))
     )    
    )
   )
))

