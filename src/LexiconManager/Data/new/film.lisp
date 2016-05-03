;;;;
;;;; w::film
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (w::film
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060711 :change-date nil :comments nil)
     (LF-PARENT ont::material)
     (example "35mm film")
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::film
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("transcribe-25.4") :wn ("film%2:32:00" "film%2:36:00"))
     (LF-PARENT ONT::record)
 ; like tape,record
     )
    )
   )
))

