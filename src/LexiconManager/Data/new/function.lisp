;;;;
;;;; W::FUNCTION
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::FUNCTION
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date 20050817 :wn ("function%1:07:00") :comments lf-restructuring)
     (LF-PARENT ONT::utility)
     (example "the function of this program is to optimize performance")
     (TEMPL OTHER-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  (W::function
   (SENSES
    ((LF-PARENT ONT::FUNCTION)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL neutral-TEMPL)
     (meta-data :origin "verbnet-2.0" :entry-date 20060606 :change-date nil :comments nil :vn ("masquerade-29.6"))
     (example "the truck functions")
     )
    )
   )
))

