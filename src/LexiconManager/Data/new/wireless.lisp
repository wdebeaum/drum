;;;;
;;;; W::wireless
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
     (W::wireless
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060426 :change-date nil :wn ("wireless%1:10:01") :comments nil)
     (example "does the hotel have free wireless access")
     (LF-PARENT ONT::WIRELESS)
     (templ mass-pred-templ)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::WIRELESS
    (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo :entry-date 20030605 :change-date nil :wn ("wireless%3:00:00") :comments calo-y1script)
     (LF-PARENT ONT::WIRELESS)
     )
    )
   )
))

