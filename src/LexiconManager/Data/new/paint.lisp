;;;;
;;;; W::PAINT
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
 (W::PAINT
   (SENSES
    ((meta-data :origin fruit-carts :entry-date 20041103 :change-date nil :comments nil)
     (lf-parent ont::coloring)
     (SEM (F::aspect F::bounded) (F::time-span F::atomic))
     (example "paint the triangle")
     )
    #||((meta-data :origin fruit-carts :entry-date 20041103 :change-date nil :comments nil)
     (lf-parent ont::coloring)
     (SEM (F::aspect F::bounded) (F::time-span F::atomic))
     (example "paint the triangle red")
     (TEMPL agent-affected-result-optional-templ (xp (% w::ADJP  (w::set-modifier -))))
     )  ||#
    )
   )
))

(define-words :pos W::n
 :tags (:base500)
 :words (
  (W::PAINTING
   (SENSES
    ( (LF-PARENT ONT::IMAGE)
     (TEMPL OTHER-RELN-TEMPL)
     )
    )
   ))
 )
   
