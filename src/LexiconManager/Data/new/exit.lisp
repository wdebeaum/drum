;;;;
;;;; W::EXIT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::EXIT
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("exit%1:06:00"))
     (LF-PARENT ONT::exit)
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
(W::exit
   (wordfeats (W::morph (:forms (-vb) :past W::exited :ing W::exiting)))
   (SENSES
    ((meta-data :origin plot :entry-date 20080413 :change-date nil :comments nil)
     (LF-PARENT ONT::depart)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (templ agent-neutral-optional-templ (xp (% W::NP)))
     (example "exit the room")
     )
    ((meta-data :origin plot :entry-date 20080413 :change-date nil :comments nil)
     (LF-PARENT ONT::stop)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (templ agent-affected-xp-templ)
     (example "exit the \"pine \" program")
     )
    )
   )
))

