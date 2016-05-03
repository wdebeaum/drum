;;;;
;;;; W::transmit
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::transmit
    (wordfeats (W::morph (:forms (-vb) :nom W::transmission)))
   (SENSES
    ((LF-PARENT ONT::SEND)
     (example "transmit the image to him" "transmit him the image")
     (templ agent-affected-recipient-alternation-templ)
     (SEM (F::aspect F::bounded) (F::time-span F::atomic))
     (meta-data :origin task-learning :entry-date 20050830 :change-date nil :comments nil :vn ("send-11.1-1"))
     )
    )
   )
))

