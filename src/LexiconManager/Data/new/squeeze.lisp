;;;;
;;;; W::squeeze
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::squeeze
   (wordfeats (W::morph (:forms (-vb) :nom w::squeeze)))
   (SENSES
    ((LF-PARENT ONT::squeeze)
     (example "the engine squeezes gasoline from the line")
     (meta-data :origin mobius :entry-date 20080414 :change-date 20090529 :comments nil)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (SEM (F::ASPECT F::DYNAMIC))
     )
    )
   )
))

