;;;;
;;;; W::calibrate
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(W::calibrate
   (wordfeats (W::morph (:forms (-vb) :nom w::calibration)))
   (SENSES
    ((LF-PARENT ONT::device-adjust)
     (example "calibrate the display")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin task-learning :entry-date 20050826 :change-date nil :comments nil)
     )
    ))
))

