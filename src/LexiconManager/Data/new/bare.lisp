;;;;
;;;; w::bare
;;;; 

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ((W::BARE W::BONES)
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo :entry-date 20031223 :change-date nil :wn ("basic%3:00:00") :comments html-purchasing-corpus)
     (lf-parent ont::basic-val)
     (example "a bare bones system")
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::bare
    (wordfeats (W::MORPH (:FORMS (-er -LY))))
   (SENSES
    ((LF-PARENT ONT::UNADORNED)
     (example "the bare essentials")
     (meta-data :origin cardiac :entry-date 20080508 :change-date 20090731 :comments LM-vocab)
     )
    ((LF-PARENT ont::not-clothed-val)
     (example "He was bare from waist up.")
     (meta-data :origin adjective-reorganization :entry-date 20170317 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::adj
 :words (
  (w::bare
  (senses
   ((LF-PARENT ONT::basic-VAL)
    (TEMPL central-adj-templ)
    (meta-data :origin cardiac :entry-date 20080520 :change-date nil :comments nil)
    (SYNTAX (W::morph (:forms (-ly))))
    )
   )
)
))

