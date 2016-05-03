;;;;
;;;; W::UNIT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  ;; a unit of measure, a unit of electricity
  (W::UNIT
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060713 :change-date nil :wn ("unit%1:23:00") :comments caloy3)
     (LF-PARENT ONT::MEASURE-UNIT)
     (example "a word is a linguistic unit")
     (TEMPL other-reln-templ)
     )
    ((LF-PARENT ONT::group-object)
     (TEMPL count-pred-templ)
     (syntax (agr (? ag 3s 3p)))
     (SEM (F::intentional +))
     (example "dispatch unit 7")
     (meta-data :origin obtw :entry-date 20110922 :change-date nil :comments demo)
     )
    )
   )
))

