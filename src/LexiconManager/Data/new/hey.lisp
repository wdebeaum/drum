;;;;
;;;; W::hey
;;;;

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
   ((W::hey W::there)
    (meta-data :origin asma :entry-date 20111020 :change-date nil :comments nil)
   (SENSES
    ((LF (W::HELLO))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_GREET))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  (W::hey
   (SENSES
    ((LF (W::ATTENTION))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_EXPRESSIVE))
     )
    ((LF (W::HELLO))
     (meta-data :origin asma :entry-date 20111020 :change-date nil :comments nil)
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_GREET))
     )
    )
   )
))

