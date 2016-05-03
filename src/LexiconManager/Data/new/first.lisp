;;;;
;;;; W::FIRST
;;;;

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::FIRST
   (SENSES
    ((LF-PARENT ONT::SEQUENCE-POSITION)
     (TEMPL PRED-S-VP-TEMPL)
     ;;(PREFERENCE 0.98)
     )
    ((LF-PARENT ONT::SEQUENCE-POSITION)
     (TEMPL DISC-TEMPL)
     (example "first open the browser window")
     )))

  ((W::FIRST w::of w::all)
   (SENSES
    ((LF-PARENT ONT::SEQUENCE-POSITION)
     (meta-data :origin caet :entry-date 20130523 :change-date nil :comments icmi)
     (TEMPL PRED-S-TEMPL)
     )
    
    ((LF-PARENT ONT::SEQUENCE-POSITION)
     (meta-data :origin caet :entry-date 20130523 :change-date nil :comments icmi)
     (TEMPL DISC-TEMPL)
     (example "first of all open the browser window")
     )
    )
   )

))



(define-words :pos W::ORDINAL :boost-word t :templ ORDINAL-TEMPL
 :tags (:base500)
 :words (
  (W::FIRST
   (SENSES
    ((LF (W::NTH 1))
     (non-hierarchy-lf t)(SYNTAX (W::agr ?a) (w::ntype w::day))
     )
    )
   )
))

