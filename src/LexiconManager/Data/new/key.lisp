;;;;
;;;; W::key
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::key
   (SENSES
    ((LF-PARENT ONT::identification)
     (EXAMPLE "type in the identification key")
     (meta-data :origin task-learning :entry-date 20050824 :change-date nil :comments nil)
     )
    ((LF-PARENT ONT::device) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("key%1:06:00")
      :COMMENTS HTML-PURCHASING-CORPUS)
     (EXAMPLE "he lost his house key")
     )
    )
   )
))

