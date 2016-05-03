;;;;
;;;; W::constraint
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::constraint
   (wordfeats (W::MORPH (:forms (-S-3P))))
   (SENSES
    ((EXAMPLE "there is a scheduling constraint (that you must take this before meals)")
     (meta-data :origin medadvisor :entry-date 20020610 :change-date 20041201 :comments nil :wn ("constraint%1:04:00"))
     (LF-PARENT ONT::CONSTRAINT)
     (templ count-subcat-that-optional-templ)
     )
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("constraint%1:04:00"))
     (EXAMPLE "Is there a constraint on taking celebrex?")
     (LF-PARENT ONT::CONSTRAINT)
     (TEMPL COUNT-PRED-SUBCAT-TEMPL (XP (% W::PP (W::PTYPE W::on))))
     )
    )
   )
))

