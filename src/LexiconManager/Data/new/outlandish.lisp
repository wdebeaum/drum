;;;;
;;;; W::outlandish
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::outlandish
   (wordfeats (W::morph (:FORMS ( -LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090129 :change-date 20090731 :wn ("strange%3:00:00") :comments LM-vocab)
     (EXAMPLE "A freaky idea")
     (LF-PARENT ONT::STRANGE)
     (sem (f::gradability +) (f::intensity ont::hi) (f::orientation ont::less))
     )
    )
   )
))

(define-words :pos W::adj
 :words (
  (w::outlandish
  (senses
   ((LF-PARENT ONT::strange)
    (TEMPL central-adj-templ)
    (sem (f::gradability +) (f::intensity ont::med) (f::orientation ont::less))
    (meta-data :origin cardiac :entry-date 20090129 :change-date 20090804 :wn ("strange%3:00:00") :comments nil)
    )
   )
)
))

