
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::suitable
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin caloy2 :entry-date 20050418 :change-date 20061106 :wn ("suitable%5:00:00:appropriate:00") :comments projector-purchasing :comlex (ADJECTIVE))
     (example "a good book")
     (lf-parent ont::appropriate-val)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::med))
     (TEMPL central-adj-templ)
     )
    ((meta-data :origin caloy2 :entry-date 20050418 :change-date 20061106 :wn ("suitable%5:00:00:appropriate:00") :comments projector-purchasing :comlex (ADJECTIVE))
     (example "a wall good for climbing")
     (lf-parent ont::appropriate-val)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::med))
     (TEMPL adj-purpose-TEMPL)
     )
    ((meta-data :origin caloy2 :entry-date 20050418 :change-date 20061106 :wn ("suitable%5:00:00:appropriate:00") :comments projector-purchasing :comlex (ADJECTIVE))
     (EXAMPLE "a drug suitable for cancer")
     (lf-parent ont::appropriate-val)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::med))
     (TEMPL adj-purpose-implicit-XP-templ)
     )
    ((meta-data :origin caloy2 :entry-date 20050418 :change-date 20061106 :wn ("suitable%5:00:00:appropriate:00") :comments projector-purchasing :comlex (ADJECTIVE))
     (EXAMPLE "a solution good for him")
     (lf-parent ont::appropriate-val)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::med))
     (TEMPL adj-affected-XP-templ)
     )
    )
   )
))

