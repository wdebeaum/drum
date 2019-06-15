;;;;
;;;; W::broadcast
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::broadcast
   (wordfeats (W::morph (:forms (-vb) :pastpart w::broadcast)))
   (SENSES
    ((LF-PARENT ONT::SEND)
     (meta-data :origin task-learning :entry-date 20050825 :change-date nil :comments nil)
     (example "broadcast the website to the network")
     (SEM (F::aspect F::bounded) (F::time-span F::atomic))
     )
    )
   )
))

