;;;;
;;;; W::powerful
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::powerful
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo :entry-date 20031205 :change-date 20090731 :wn ("powerful%3:00:00") :comments calo-y1script)
     (LF-PARENT ONT::intense)
     (sem (f::gradability +) (f::intensity ont::hi) (f::orientation ont::less))
     )
    )
   )
))

