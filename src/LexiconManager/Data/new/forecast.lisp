;;;;
;;;; W::forecast
;;;;

#||(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::forecast
   (SENSES
    ((LF-PARENT ONT::predict)
     (example "click here for the forecast")
     (meta-data :origin task-learning :entry-date 20051109 :change-date nil :wn ("forecast%1:10:00") :comments zipcode-dialog)
     (templ other-reln-theme-templ)
     )
    )
   )
))||#

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::forecast
  (wordfeats (W::morph (:forms (-vb) :past W::forecast :nom w::forecast)))
   (SENSES
    ((LF-PARENT ONT::predict)
     (preference .97) ;; prefer noun sense
     (SEM (F::Time-span F::extended))
     (TEMPL agent-neutral-xp-templ)
     (example "the weatherman forecasts rain [for this zipcode]")
     )
    )
   )
))

