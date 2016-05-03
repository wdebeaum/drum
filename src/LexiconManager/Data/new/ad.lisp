;;;;
;;;; W::AD
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::AD
   (SENSES
    ((LF-PARENT ONT::information) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("ad%1:10:00")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   ((w::ad w::hoc)
   (SENSES
    ((meta-data :origin task-learning :entry-date 20060524 :change-date nil :wn ("ad_hoc%5:00:00:unplanned:00" "ad_hoc%5:00:00:specific:00") :comments nil)
     (example "an ad hoc solution")
     (LF-PARENT ONT::status-val)
     )
    )
   )
))

(define-words :pos W::value :boost-word t
 :words (
  (w::ad
  (senses
   ((LF-PARENT ONT::era)
    (TEMPL value-templ) (PREFERENCE 0.92)
    )
   )
)
))

