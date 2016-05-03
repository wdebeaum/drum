;;;;
;;;; W::squeeze
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::squeeze
   (wordfeats (W::morph (:forms (-vb) :nom w::squeeze)))
   (SENSES
    ((LF-PARENT ONT::squeeze)
     (example "the engine squeezes gasoline from the line")
     (meta-data :origin mobius :entry-date 20080414 :change-date 20090529 :comments nil)
     (templ agent-affected-xp-templ)
     (SEM (F::ASPECT F::DYNAMIC))
     )
    )
   )
))

