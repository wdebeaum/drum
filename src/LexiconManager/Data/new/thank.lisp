;;;;
;;;; W::thank
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::thank
   (SENSES
    ;; the for phrase is ont::reason, generated in the grammar
    ((LF-PARENT ONT::thank)
     (example "thank you [for your help]")
     (TEMPL AGENT-ADDRESSEE-TEMPL)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::thank W::you)
   (SENSES
    ((LF (W::THANKS))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_THANK))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::thank W::you W::very W::much)
   (SENSES
    ((LF (W::THANKS))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_THANK))
     )
    )
   )
))

