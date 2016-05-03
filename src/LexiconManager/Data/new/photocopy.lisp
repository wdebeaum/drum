;;;;
;;;; W::photocopy
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::photocopy
    (SENSES
    ((meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :wn ("photocopy%1:06:00") :comments Office)
     (example "make a photocopy")
     (LF-PARENT ONT::phys-representation)
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::photocopy
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("transcribe-25.4") :wn ("photocopy%2:36:00"))
     (LF-PARENT ONT::record)
 ; like tape,record
     )
    )
   )
))

