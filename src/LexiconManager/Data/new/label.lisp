;;;;
;;;; W::label
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::label
   (SENSES
    ((LF-PARENT ONT::information-function-object)
     (EXAMPLE "read the label")
     (meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :wn ("label%1:10:01") :comments Office)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::label
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090501 :comments nil :vn ("dub-29.3-1"))
     (LF-PARENT ONT::naming)
     (TEMPL AGENT-NEUTRAL-FORMAL-2-XP-3-XP2-NP-OPTIONAL-TEMPL) ; like name
     (PREFERENCE 0.96)
     )
     )
   )
))

