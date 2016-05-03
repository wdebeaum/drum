;;;;
;;;; W::pan
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::pan
   (SENSES
    ((LF-PARENT ONT::cookware)
     (meta-data :origin caloy3 :entry-date 20070330 :change-date nil :comments y3-test-data)
     (preference .85) ;; need very low preference to allow preferred coordops interp: "red pan camera full left
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (w::pan
   (senses
    ((LF-PARENT ONT::pan)
     (meta-data :origin lou :entry-date 20040319 :change-date nil :comments lou-sent-entry)
     (example "pan the camera up a little")
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     )
     ((LF-PARENT ONT::pan)
     (meta-data :origin lou :entry-date 20041027 :change-date nil :comments lou-sent-entry)
     (example "pan up a little")
     (templ agent-templ)
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     )
    )
   )
))

