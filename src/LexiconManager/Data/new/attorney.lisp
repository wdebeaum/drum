;;;;
;;;; W::attorney
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((W::attorney w::general)
   (SENSES
    ((meta-data :origin mrs :entry-date 20061027 :change-date nil :comments nil)
     (LF-PARENT ONT::legal-professional) ;professional)
     (templ other-reln-templ)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::attorney
   (SENSES
    ((meta-data :origin mrs :entry-date 20061027 :change-date nil :comments nil :wn ("attorney%1:18:00"))
     (LF-PARENT ONT::legal-professional) ;professional)
     (templ other-reln-templ)
     )
    )
   )
))

