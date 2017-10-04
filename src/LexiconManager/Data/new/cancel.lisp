;;;;
;;;; W::cancel
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::cancel
   (SENSES
    ((LF-PARENT ONT::CANCEL)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-affected-XP-TEMPL)
     (example "cancel the meeting")
     )
    
     ((LF-PARENT ONT::CANCEL)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-TEMPL)
     (preference .95) ;; prefer transitive senses
     (example "cancel")
     (meta-data :origin plot :entry-date 20081106 :change-date nil :comments nil)
     )
    )
   )
))

#|
(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::cancel W::it)
   (SENSES
    ((LF (W::FORGET-IT))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_REJECT))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::cancel W::that)
   (SENSES
    ((LF (W::FORGET-IT))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_REJECT))
     )
    )
   )
))

|#


