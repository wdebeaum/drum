;;;;
;;;; W::application
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::application
   (SENSES
    ((LF-PARENT ONT::template-info-object)
     (example "fill out the application")
     (meta-data :origin plow :entry-date 20050325 :change-date nil :wn ("application%1:10:00") :comments nil)
     )
#|
    ((LF-PARENT ONT::use)
     (TEMPL other-reln-theme-TEMPL)
     (example "the application of the technology")
     (meta-data :origin step :entry-date 20080529 :change-date nil :comments nil :wn ("application%1:04:02")))
|#
    ((LF-PARENT ONT::computer-program)
     (example "quit the application")
     (meta-data :origin plot :entry-date 20080413 :change-date nil :wn ("application%1:10:00") :comments nil)
     )
    )
   )
))

