;;;;
;;;; W::naked
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::naked
     (wordfeats (W::MORPH (:FORMS (-LY))))
   (SENSES
    ((lf-parent ont::basic-val)
     (example "the naked truth")
     (meta-data :origin cardiac :entry-date 20080508 :change-date 20090731 :comments LM-vocab)
     )
    ((lf-parent ont::bare-val)
     (example "the naked boy")
     )
    )
   )
))

