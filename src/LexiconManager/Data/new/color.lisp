;;;;
;;;; W::COLOR
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::COLOR
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::color)
     (TEMPL OTHER-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
 (W::color
   (SENSES
    ((meta-data :origin fruit-carts :entry-date 20050331 :change-date nil :comments nil)
     (lf-parent ont::coloring)
     (SEM (F::aspect F::bounded) (F::time-span F::atomic))
     (example "color the triangle")
     )
    ;; not done with a general result rule
   #|| ((meta-data :origin fruit-carts :entry-date 20050331 :change-date nil :comments nil)
     (lf-parent ont::coloring)
     (SEM (F::aspect F::bounded) (F::time-span F::atomic))
     (example "color the triangle red")
     (TEMPL agent-affected-result-optional-templ (xp (% w::ADJP  (w::set-modifier -))))
     )   ||# 
    )
   )
))

