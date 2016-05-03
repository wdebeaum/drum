;;;;
;;;; W::transfer
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::transfer
   (wordfeats (W::MORPH (:forms (-S-3P))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("transfer%1:04:00"))
     (EXAMPLE "It's a brain transfer")
     (LF-PARENT ONT::transfer-event)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::transfer
   ;; adding the morph forms as the doubled consonant with unstressed final syllable is an exception to the morph rule
   (wordfeats (W::morph (:forms (-vb) :past W::transferred :ing W::transferring)))
   (SENSES
    ((LF-PARENT ONT::transfer)
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (EXAMPLE "transfer the message to another mailbox")
     (meta-data :origin task-learning :entry-date 20050823 :change-date nil :comments nil)
     )
    )
   )
))

