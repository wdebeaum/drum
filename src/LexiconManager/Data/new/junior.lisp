;;;;
;;;; w::junior
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
 ((w::junior w::high)
   (SENSES
    ((LF-PARENT ONT::academic-institution)
     (EXAMPLE "send email from school")
     (meta-data :origin asma :entry-date 20111003 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::junior
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20051214 :change-date 20090731 :wn ("junior%3:00:00") :comments nil)
     (lf-parent ont::junior-val)
     (example "junior scientist")
     )
    )
   )
))

