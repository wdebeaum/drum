;;;;
;;;; W::CARBON
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::CARBON
   (SENSES
    ((LF-PARENT ONT::substance) (TEMPL mass-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("carbon%1:27:00")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::carbon w::dioxide)
   (SENSES
    (
     (LF-PARENT ONT::natural-gas-substance)
     (SEM (F::form F::gas))
     (TEMPL MASS-PRED-TEMPL)
     )
    )
   )
))
