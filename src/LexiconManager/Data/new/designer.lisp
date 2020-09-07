;;;;
;;;; W::designer
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::designer
   (SENSES
    ((LF-PARENT ONT::professional) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
;   )
  (W::designer
   (SENSES
    ((LF-PARENT ONT::artist) ;professional)
     (EXAMPLE "designers create Web pages")
     (meta-data :origin task-learning :entry-date 20050830 :change-date nil :wn ("designer%1:18:00") :comments nil)
     )
    )
   )
))

