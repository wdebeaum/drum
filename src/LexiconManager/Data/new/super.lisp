
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::super
    (SENSES
     ((meta-data :origin calo :entry-date 20031223 :change-date 20061106 :wn ("super%5:00:00:superior:00") :comments html-purchasing-corpus :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
      (example "a good book")
      (lf-parent ont::great-val)
      (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::hi))
      (TEMPL central-adj-templ)
      )
     ((meta-data :origin calo :entry-date 20031223 :change-date 20061106 :wn ("super%5:00:00:superior:00") :comments html-purchasing-corpus :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
      (example "a wall good for climbing")
      (lf-parent ont::great-val)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::hi))
     (TEMPL adj-purpose-TEMPL)
     )
     ((meta-data :origin calo :entry-date 20031223 :change-date 20061106 :wn ("super%5:00:00:superior:00") :comments html-purchasing-corpus :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
      (EXAMPLE "a drug suitable for cancer")
      (lf-parent ont::great-val)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::hi))
     (TEMPL adj-purpose-implicit-XP-templ)
     )
     ((meta-data :origin calo :entry-date 20031223 :change-date 20061106 :wn ("super%5:00:00:superior:00") :comments html-purchasing-corpus :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
      (EXAMPLE "a solution good for him")
      (lf-parent ont::great-val)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::hi))
     (TEMPL adj-affected-XP-templ)
     )
     )
    ) 
))

