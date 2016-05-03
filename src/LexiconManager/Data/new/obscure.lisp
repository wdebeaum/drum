;;;;
;;;; W::obscure
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::obscure
   (SENSES
    ((meta-data :origin task-learning :entry-date 20050919 :change-date 20090731 :wn ("obscure%5:00:00:concealed:00" "obscure%5:00:00:unclear:00") :comments nil)
     (LF-PARENT ONT::hidden)
     (EXAMPLE "this document contains explanatory material for obscure commands")
     )
    )
   )
))

(define-words :pos W::adj
 :words (
  (w::obscure
  (senses
   ((LF-PARENT ONT::unclear)
    (TEMPL central-adj-templ)
    (sem (f::gradability +) (f::intensity ont::med) (f::orientation ont::less))
    (meta-data :origin cardiac :entry-date 20090129 :change-date 20090804 :comments nil)
    (SYNTAX (W::morph (:forms (-ly))))
    )
   )
)
))

