;;;;
;;;; W::control
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::control
   (SENSES
    ((meta-data :origin calo :entry-date 20050308 :change-date nil :wn ("control%1:06:00") :comments projector-purchasing)
     (example "we need the remote control")
     (LF-PARENT ONT::device)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
(W::control
   (wordfeats (W::morph (:forms (-vb) :nom W::control)))
   (SENSES
    #||((LF-PARENT ONT::control-manage)  ;; subsumed by cause template
     (example "control the process")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-affected-XP-TEMPL)
      (meta-data :origin calo :entry-date 20050527 :change-date nil :comments projector-purchasing)
     )||#
    ((LF-PARENT ONT::control-manage)
     (example "the piston controls the cylinder")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-affected-xp-templ)
     (meta-data :origin LbR :entry-date 20080922 :change-date nil :comments nil)
     )
    )
   )
))

