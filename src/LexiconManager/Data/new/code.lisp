;;;;
;;;; W::code
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::code
   (SENSES
   ((EXAMPLE "put the airport code here")
     (meta-data :origin calo :entry-date 20050816 :change-date nil :comments pq0404)
     (LF-PARENT ONT::identification)
     )
    ((EXAMPLE "write some code")
     (meta-data :origin calo :entry-date 20050816 :change-date nil :wn ("code%1:10:02") :comments html-purchasing-corpus)
     (LF-PARENT ONT::computer-software)
     (TEMPL mass-pred-TEMPL)
     (preference .94)
     )
    )
   )
))

