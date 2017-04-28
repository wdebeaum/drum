;;;;
;;;; W::moderate
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::moderate
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((lf-parent ont::moderate-val)
     (example "under moderate conditions")
     (meta-data :origin calo :entry-date 20041122 :change-date nil :wn ("extreme%5:00:00:intense:00") :comments caloy2)
     (sem (f::gradability +) (f::intensity ont::med) (f::orientation ont::less))
     )
    )
   )
))

