;;;;
;;;; W::definite
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::definite
   (SENSES
    ((LF-PARENT ONT::confidence-VAL)
     (example "he is certain that he will come to the party")
     (TEMPL CENTRAL-ADJ-XP-TEMPL (XP (% W::cp (W::ctype W::s-finite))))
     (meta-data :origin calo :entry-date 20090424 :change-date nil :wn ("certain%3:00:02"))
     )
    )
   )
))

