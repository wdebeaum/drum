;;;;
;;;; W::flip
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::flip (w::over))
   (SENSES
    ((LF-PARENT ONT::ROTATE)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin fruit-carts :entry-date 20050401 :change-date nil :comments nil)
     (example "flip over the triangle")
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::flip
   (SENSES
    ((LF-PARENT ONT::ROTATE)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin fruit-carts :entry-date 20050401 :change-date nil :comments fruitcarts-07-2)
     (example "flip the triangle to the other side")
     )
    )
   )
))

