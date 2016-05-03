;;;;
;;;; W::mine
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
   (W::mine
   (SENSES
    ((LF-PARENT ONT::manufactured-object)
     (meta-data :origin lou :entry-date 20040311 :change-date nil :wn ("mine%1:06:00") :comments lou-sent-entry)
     )
    )
   )
))

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
   (W::MINE
   (wordfeats (W::agr W::1s) (W::stem W::me))
   (SENSES
    ((LF-PARENT ONT::PERSON)
     (TEMPL poss-pronoun-templ)
     (example "it is mine")
     )
    )
   )
))

