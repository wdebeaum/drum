;;;;
;;;; W::EMERGENCY
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::EMERGENCY W::ROOM)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::emergency W::rooms))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("emergency_room%1:06:00"))
     (LF-PARENT ONT::health-care-facility)
     (LF-FORM W::emergency-room)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::EMERGENCY
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("emergency%1:11:00"))
     (LF-PARENT ONT::LOCATED-EVENT)
     )
    )
   )
))

