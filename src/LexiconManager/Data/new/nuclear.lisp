;;;;
;;;; W::nuclear
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::nuclear
   (SENSES
    ((LF-PARENT ONT::substantial-property-val)
     (meta-data :origin caloy3 :entry-date 20070330 :change-date 20090915 :comments y3-test-data)
     )

    (
     (LF-PARENT ONT::BODY-PART-VAL)
     (SYNTAX (w::pertainym (:* ONT::NUCLEUS W::NUCLEUS)))
     (TEMPL CENTRAL-ADJ-TEMPL)
     )
    )
   )
))

