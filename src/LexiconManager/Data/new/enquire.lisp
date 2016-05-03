;;;;
;;;; W::enquire
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::enquire
   (SENSES
    ((EXAMPLE "enquire about the voltage")
     (meta-data :origin calo :entry-date 20050425 :change-date nil :comments projector-purchasing)
     ;;(LF-PARENT ONT::QUESTIONING)
     ;;(lf-parent ont::enquire-inquire) ;; 20120524 GUM change new parent
     (LF-PARENT ONT::ASK-QUESTION)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-ASSOCIATED-INFORMATION-TEMPL)
     )
    )
   )
))

