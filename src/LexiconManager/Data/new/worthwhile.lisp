
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::WORTHWHILE
   (SENSES
    ((meta-data :origin calo :entry-date 20040915 :change-date 20061106 :wn ("worthwhile%5:00:00:worthy:00") :comments caloy2  :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (example "a good book")
     (lf-parent ont::worthy-val)
     (TEMPL central-adj-templ)
     )
    ((meta-data :origin calo :entry-date 20040915 :change-date 20061106 :wn ("worthwhile%5:00:00:worthy:00") :comments caloy2  :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (example "a wall good for climbing")
     (lf-parent ont::worthy-val)
     (TEMPL adj-purpose-TEMPL (XP (% W::PP (W::PTYPE W::FOR))))
     )
    ((meta-data :origin calo :entry-date 20040915 :change-date 20061106 :wn ("worthwhile%5:00:00:worthy:00") :comments caloy2  :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (EXAMPLE "a drug suitable for cancer")
     (lf-parent ont::worthy-val)
     (TEMPL adj-purpose-implicit-XP-templ)
     )
    ((meta-data :origin calo :entry-date 20040915 :change-date 20061106 :wn ("worthwhile%5:00:00:worthy:00") :comments caloy2  :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (EXAMPLE "a solution good for him")
     (lf-parent ont::worthy-val)
     (TEMPL adj-affected-XP-templ)
     )    
    )
   )
))

