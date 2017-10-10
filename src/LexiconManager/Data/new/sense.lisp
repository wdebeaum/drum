;;;;
;;;; W::SENSE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::SENSE
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("sense%1:09:04"))
     (LF-PARENT ONT::mental-object)
     (LF-FORM W::sense)
     (TEMPL MASS-PRED-TEMPL)
     (EXAMPLE "that makes sense")
     )
    ((meta-data :origin calo-ontology :entry-date 20060320 :change-date nil :wn ("sense%1:09:02") :comment verbnet-expansion)
     (LF-PARENT ONT::ability-to-perceive)
     (example "good food tantalizes the senses")
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::sense
   (SENSES
    ((LF-PARENT ONT::information)
     (EXAMPLE "what is the sense of this word")
     (meta-data :origin calo-ontology :entry-date 20060119 :change-date nil :wn ("sense%1:10:00") :comments caloy3)
     )
    )
   )
))

(define-words :pos W::V 
 :words (
  (W::sense
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("see-30.1-1"))
     (LF-PARENT ONT::suppose)
     (TEMPL experiencer-theme-xp-templ (xp (% w::cp (w::ctype w::s-finite)))) ; like see
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("see-30.1") :wn ("sense%2:39:00"))
     (LF-PARENT ONT::active-perception)
     (TEMPL experiencer-neutral-templ) ; like smell,taste
     )
    )
   )
))

