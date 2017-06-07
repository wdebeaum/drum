;;;;
;;;; W::sanction
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::sanction
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090121 :change-date 20090507 :comments nil :vn ("judgement-33"))
     ;(LF-PARENT ont::allow)
     (LF-PARENT ONT::approve-authorize)
     (example "he sanctioned their proposal")
     (TEMPL agent-affected-xp-templ) 
     )
    )
   )
))

