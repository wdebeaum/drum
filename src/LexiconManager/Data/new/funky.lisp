;;;;
;;;; W::funky
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::funky
   (wordfeats (W::morph (:FORMS (-er -LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090731 :comments LM-vocab)
     (EXAMPLE "A funky idea")
     (LF-PARENT ONT::STRANGE)
     (sem (f::gradability +) (f::intensity ont::med) (f::orientation ont::less))
     )
    )
   )
))

