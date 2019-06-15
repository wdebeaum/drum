;;;;
;;;; W::inquire
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::inquire
  (wordfeats (W::morph (:forms (-vb) :nom W::inquiry)))
   (SENSES
    ((EXAMPLE "inquire about the voltage")
     (meta-data :origin calo :entry-date 20050425 :change-date nil :comments projector-purchasing)
     ;;(LF-PARENT ONT::QUESTIONING)
     ;;(lf-parent ont::enquire-inquire) ;; 20120524 GUM change new parent
     (LF-PARENT ONT::ASK-QUESTION)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-TEMPL)
     )
    )
   )
))

