
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::inappropriate
    (wordfeats (W::morph (:FORMS (-LY))))
    (SENSES
     ((meta-data :origin calo :entry-date 20031223 :change-date 20061106 :wn ("inappropriate%3:00:00" "inappropriate%5:00:00:incongruous:00") :comments html-purchasing-corpus :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (example "a good book")
     (lf-parent ont::not-appropriate-val)
     (sem (f::gradability +) (f::orientation ont::less) (f::intensity ont::med))
     (TEMPL central-adj-templ)
     )
     ((meta-data :origin calo :entry-date 20031223 :change-date 20061106 :wn ("inappropriate%3:00:00" "inappropriate%5:00:00:incongruous:00") :comments html-purchasing-corpus :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
      (example "a wall good for climbing")
      (lf-parent ont::not-appropriate-val)
      (sem (f::gradability +) (f::orientation ont::less) (f::intensity ont::med))
      (TEMPL adj-purpose-TEMPL)
      )
     ((meta-data :origin calo :entry-date 20031223 :change-date 20061106 :wn ("inappropriate%3:00:00" "inappropriate%5:00:00:incongruous:00") :comments html-purchasing-corpus :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
      (EXAMPLE "a drug suitable for cancer")
      (lf-parent ont::not-appropriate-val)
      (sem (f::gradability +) (f::orientation ont::less) (f::intensity ont::med))
      (TEMPL adj-purpose-implicit-XP-templ)
      )
     ((meta-data :origin calo :entry-date 20031223 :change-date 20061106 :wn ("inappropriate%3:00:00" "inappropriate%5:00:00:incongruous:00") :comments html-purchasing-corpus :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
      (EXAMPLE "a solution good for him")
      (lf-parent ont::not-appropriate-val)
      (sem (f::gradability +) (f::orientation ont::less) (f::intensity ont::med))
      (TEMPL adj-affected-XP-templ)
      )
     )
    )
))

