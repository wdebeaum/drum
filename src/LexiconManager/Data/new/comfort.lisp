;;;;
;;;; W::comfort
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::comfort
   (SENSES
    ((LF-PARENT ONT::comfortable-scale)
     (EXAMPLE "comfort is also an important attribute in hotel selection")
     (meta-data :origin calo :entry-date 20060117 :change-date nil :wn ("comfort%1:26:00") :comments calo-ontology)
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::comfort
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090511 :comments nil :vn ("amuse-31.1") :wn ("comfort%2:37:01"))
     (LF-PARENT ONT::evoke-comfort) ;evoke-relief)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

