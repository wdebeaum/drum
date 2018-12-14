;;;;
;;;; W::sweat
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::sweat
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date 20070827 :comments nil :wn ("sweat%1:08:00"))
     (LF-PARENT ont::bodily-fluid)
     (TEMPL MASS-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((w::sweat w::pants)
  (senses
   ((LF-PARENT ONT::attire)
    (meta-data :origin caloy3 :entry-date 20070330 :change-date nil :comments nil)
    (TEMPL mass-PRED-TEMPL)
    (syntax (W::morph (:forms (-none))))
    )
   )
)
))

(define-words :pos W::n
 :words (
  ((w::sweat w::shirt)
  (senses
   ((LF-PARENT ONT::attire)
    (meta-data :origin caloy3 :entry-date 20070330 :change-date nil :comments nil)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::n
 :words (
  ((w::sweat w::suit)
  (senses
   ((LF-PARENT ONT::attire)
    (meta-data :origin caloy3 :entry-date 20070330 :change-date nil :comments nil)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::V 
 :words (
  (W::sweat
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("breathe-40.1.2") :wn ("sweat%2:29:00"))
     (LF-PARENT ONT::sweat)
     (example "he sweated blood")
     (TEMPL affected-affected-templ) ; like vomit
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("breathe-40.1.2") :wn ("sweat%2:29:00"))
     (LF-PARENT ONT::sweat)
     (TEMPL affected-templ) ; like bleed
     )
    )
   )
))

