;;;;
;;;; W::ALTERNATE
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::ALTERNATE
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("alternate%5:00:00:secondary:00"))
     (LF-PARENT ONT::identity-val)
     (TEMPL adj-co-theme-templ)
     (SEM (F::GRADABILITY F::-))
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  (W::alternate
   (wordfeats (W::morph (:forms (-vb) :nom w::alternation)))
   (SENSES
    ((lf-parent ont::arranging)
     (example "alternate the color of the blocks")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-affected-xp-TEMPL)
     
    )
   )
   )))

