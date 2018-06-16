;;;;
;;;; W::ALONGSIDE
;;;;

(define-words :pos W::ADV
 :words (
   (W::ALONGSIDE 
   (SENSES
    ((LF-PARENT ont::pos-extended-along-linear-area)
     (TEMPL BINARY-CONSTRAINT-TEMPL)
     (example "he found it alongside the road")
     (meta-data :origin navigation :entry-date 20080904 :change-date nil :comments nil)
     (preference .97) ;; prefer trajectory sense
     )
    #||((LF-PARENT ONT::ALONG)
     (TEMPL BINARY-CONSTRAINT-TEMPL)
     (example "he walked alongside the road")
     (meta-data :origin navigation :entry-date 20080904 :change-date nil :comments nil)
     )||#
    )
   )
))

(define-words :pos W::ADV
 :words (
   ((W::ALONGSIDE w::of)
   (SENSES
    ((LF-PARENT   ont::pos-extended-along-linear-area)
     (TEMPL BINARY-CONSTRAINT-TEMPL)
     (example "he found it alongside of the road")
     (meta-data :origin navigation :entry-date 20080904 :change-date nil :comments nil)
     (preference .97) ;; prefer trajectory sense
     )
    ((LF-PARENT ONT::ALONG)
     (TEMPL BINARY-CONSTRAINT-TEMPL)
     (example "he walked alongside of the road")
     (meta-data :origin navigation :entry-date 20080904 :change-date nil :comments nil)
     )
    )
   )
))

