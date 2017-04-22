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

(define-words :pos W::n :templ COUNT-PRED-TEMPL
;any of 24 major or minor diatonic scales that provide the tonal framework for a piece of music
 :words (
  (W::KEY
   (SENSES
    ((LF-PARENT ONT::KEY)
     (example "play music in the key of C.")
     (meta-data :wn ("key%1:10:00"))
     )
    )
   )
))


