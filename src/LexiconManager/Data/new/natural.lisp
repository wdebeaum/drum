;;;;
;;;; W::natural
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
 (W::natural
     (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060425 :change-date nil :wn ("natural%3:00:01") :comments nil)
     (EXAMPLE "an earthquake is a natural disaster")
     (LF-PARENT ONT::natural)
     (TEMPL central-adj-TEMPL)
     (SEM (F::GRADABILITY +))
     )
    )
   )
))

