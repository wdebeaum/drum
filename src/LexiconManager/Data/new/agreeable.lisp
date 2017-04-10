
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::agreeable
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090129  :change-date nil :wn ("pleasant%3:00:00") :comments weather :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))     
     (example "an agreeable book")
     (lf-parent ont::pleasing-val)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::med))
     (TEMPL central-adj-templ)
     )
 ((meta-data :origin cardiac :entry-date 20090129  :change-date nil :wn ("pleasant%3:00:00") :comments weather :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT)) 
     (EXAMPLE "a porch agreeable for reading??")
     (lf-parent ont::pleasing-val)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::med))
     (TEMPL adj-purpose-implicit-XP-templ)
     )
    ((meta-data :origin cardiac :entry-date 20090129  :change-date nil :wn ("pleasant%3:00:00") :comments weather :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))     
     (EXAMPLE "a solution good for him")
     (lf-parent ont::pleasing-val)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::med))
     (TEMPL adj-affected-XP-templ)
     )
    )
   )
))

