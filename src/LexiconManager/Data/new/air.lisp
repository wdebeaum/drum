;;;;
;;;; W::air
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::air w::conditioning)
   (SENSES
    ((LF-PARENT ONT::DEVICE) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO-ontology :ENTRY-DATE 20060609 :CHANGE-DATE NIL :wn ("air_conditioning%1:06:00")
      :COMMENTS plow-req))))
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::AIR W::FIELD)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::air W::fields))))
   (SENSES
    ((LF-PARENT ONT::transportation-facility)
     (LF-FORM W::airfield)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((W::AIR W::STRIP)
    (wordfeats (W::morph (:forms (-S-3P) :plur (W::air W::strips))))
   (SENSES
    ((LF-PARENT ONT::transportation-facility)
     (LF-FORM W::airstrip)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
   (W::AIR
   (SENSES
    ((LF-PARENT ONT::natural-substance)
     (SEM (F::form F::gas))
     (TEMPL MASS-PRED-TEMPL)
     (meta-data :origin calo-ontology :entry-date 20060425 :change-date nil :wn ("air%1:27:00") :comments nil)
     )
    )
   )
))

