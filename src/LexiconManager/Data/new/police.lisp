;;;;
;;;; W::POLICE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::POLICE W::STATION)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::police W::stations))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("police_station%1:06:00"))
     (LF-PARENT ONT::public-service-facility)
     (LF-FORM W::POLICE-STATION)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::POLICE W::DEPARTMENT)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::police W::departments))))
   (SENSES
    ((meta-data :origin monroe :entry-date 20031217 :change-date nil :comments s15)
     (LF-PARENT ONT::public-service-facility)
     (LF-FORM W::POLICE-DEPARTMENT)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::POLICE
   (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::police) ;PROFESSIONAL)
     (TEMPL COUNT-PRED-3p-TEMPL)
     )
    )
   )
))

