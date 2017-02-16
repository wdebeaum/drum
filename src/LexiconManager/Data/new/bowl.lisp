;;;;
;;;; W::bowl
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::bowl
   (SENSES
    ((LF-PARENT ONT::tableware)
     
     (example "a bowl of soup")
     (meta-data :origin calo-ontology :entry-date 20060630 :change-date nil :wn ("bowl%1:06:00" "bowl%1:06:01") :comment nil)
     )
    )
   )
))

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::bowl
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("bowl%2:38:00"))
     (LF-PARENT ONT::self-locomote)
     (TEMPL agent-templ) ; like stroll,walk
     )
    )
   )
))

