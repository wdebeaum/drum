
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::spectacular
    (SENSES
     ((meta-data :origin calo :entry-date 20031223 :change-date 20061106 :wn ("spectacular%5:00:00:conspicuous:00") :comments html-purchasing-corpus :comlex (EXTRAP-ADJ-FOR-TO-INF))
      (example "a good book")
      (lf-parent ont::great-val)
      (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::med))
      (TEMPL central-adj-templ)
      )
     ((meta-data :origin calo :entry-date 20031223 :change-date 20061106 :wn ("spectacular%5:00:00:conspicuous:00") :comments html-purchasing-corpus :comlex (EXTRAP-ADJ-FOR-TO-INF))
      (example "a wall good for climbing")
      (lf-parent ont::great-val)
      (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::med))
      (TEMPL adj-purpose-TEMPL)
      )
     ((meta-data :origin calo :entry-date 20031223 :change-date 20061106 :wn ("spectacular%5:00:00:conspicuous:00") :comments html-purchasing-corpus :comlex (EXTRAP-ADJ-FOR-TO-INF))
      (EXAMPLE "a drug suitable for cancer")
      (lf-parent ont::great-val)
      (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::med))
      (TEMPL adj-purpose-implicit-XP-templ)
      )
     ((meta-data :origin calo :entry-date 20031223 :change-date 20061106 :wn ("spectacular%5:00:00:conspicuous:00") :comments html-purchasing-corpus :comlex (EXTRAP-ADJ-FOR-TO-INF))
      (EXAMPLE "a solution good for him")
      (lf-parent ont::great-val)
      (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::med))
      (TEMPL adj-affected-XP-templ)
      )
     )
    )
))

