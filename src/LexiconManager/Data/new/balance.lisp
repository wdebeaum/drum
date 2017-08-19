;;;;
;;;; W::BALANCE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::BALANCE
   (SENSES
    ((LF-PARENT ONT::balance-scale) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::balance
   (SENSES
    ((EXAMPLE "the cost doesn't balance the effort")
     (LF-PARENT ONT::object-compare)
     (SEM (F::Aspect F::static) (F::Time-span F::extended))
     (TEMPL neutral-neutral-templ)
     (meta-data :origin calo :entry-date 20050425 :change-date nil :comments projector-purchasing)
     )
    )
   )
))

