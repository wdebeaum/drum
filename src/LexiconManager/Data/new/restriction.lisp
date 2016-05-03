;;;;
;;;; W::restriction
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::restriction
   (SENSES
    ((EXAMPLE "there is a restriction (that you must take this before meals)")
     (meta-data :origin task-learning :entry-date 20050819 :change-date nil :wn ("restriction%1:09:00") :comments nil)
     (LF-PARENT ONT::CONSTRAINT)
     (templ count-subcat-that-optional-templ)
     )
    ((EXAMPLE "there are restrictions on how many messages you can keep")
     (LF-PARENT ONT::CONSTRAINT)
     (TEMPL COUNT-PRED-SUBCAT-TEMPL (XP (% W::PP (W::PTYPE W::on))))
     (meta-data :origin task-learning :entry-date 20050819 :change-date nil :wn ("restriction%1:09:00") :comments nil)
     )
    )
   )
))

