;;;;
;;;; w::become
;;;;

(define-words :pos W::v :templ AFFECTED-TEMPL
 :words (
  (w::become
   (wordfeats (W::morph (:forms (-vb) :ing W::becoming :past W::became :pastpart W::become)))
   (senses
    ((meta-data :origin monroe :entry-date 20031217 :change-date nil :comments s15)
     (lf-parent ont::become)
     (example "the weather became dangerous")
     (templ affected-pred-templ)
     )
     ((meta-data :origin csli-ts :entry-date 20070321 :change-date nil :comments nil)
     (lf-parent ont::become)
     (example "the snow became rain")
     (templ affected-pred-templ (xp (% w::NP)))
     )
    )
   )
))

