;;;;
;;;; W::with
;;;;

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::with W::regard W::to)
   (SENSES
    ((LF-PARENT ONT::associated-information)
     (example "she would like to speak with you with regard to the transportation situation")
     (TEMPL binary-constraint-s-templ)
     (meta-data :origin calo-ontology :entry-date 20060412 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::with W::respect W::to)
   (SENSES
    ((LF-PARENT ONT::associated-information)
     (example "how do they compare with respect to speed")
     (TEMPL binary-constraint-s-templ)
     (meta-data :origin calo-ontology :entry-date 20060412 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::WITH
   (SENSES
    ((LF-PARENT ONT::assoc-with)
     (example "I want a computer with 500 mb of ram")
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     )
     ((LF-PARENT ONT::MANNER)
     (example "it is thrown with a speed of 20 m/s" "they equiped him with a sword")
     (meta-data :origin step :entry-date 20081028 :change-date nil :comments step1)
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     (preference .97)
     )
    ((LF-PARENT ONT::accompaniment)
     (example "I went to the store with Pete")
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )
        )
   )))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::WITH
   (SENSES
    ((LF (W::WITH))
     (non-hierarchy-lf t))
    )
   )
))

