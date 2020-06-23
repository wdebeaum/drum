;;;;
;;;; W::freaky
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::freaky
   (wordfeats (W::morph (:FORMS (-er -LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090129 :change-date 20090731 :wn ("strange%3:00:00") :comments LM-vocab)
     (EXAMPLE "A freaky idea")
     (LF-PARENT ONT::STRANGE)
     (sem (f::gradability +) (f::intensity ont::hi) (f::orientation ont::pos))
     )
    )
   )
))

