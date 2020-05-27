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
     (TEMPL AFFECTED-FORMAL-XP-PRED-TEMPL)
     )
     ((meta-data :origin csli-ts :entry-date 20070321 :change-date nil :comments nil)
     (lf-parent ont::become)
     (example "the snow became rain")
     (TEMPL AFFECTED-FORMAL-XP-PRED-TEMPL (xp (% w::NP)))
      )
     
     ((lf-parent ont::improve)
      (preference .98)   ;; this entry is here to stop WN intreoducing it at a higher priority
      (example "the dress becomes her")
      (TEMPL agent-affected-templ)
      )
     
    )
   )
  ))

