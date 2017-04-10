
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::AWFUL
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("awful%5:00:00:bad:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-NP-OMIT))
     (example "a good book")
     (lf-parent ont::awful-val)
     (SEM (f::gradability +) (f::orientation ont::less) (f::intensity ont::hi))
     (TEMPL central-adj-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("awful%5:00:00:bad:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-NP-OMIT))
     (example "a wall good for climbing")
     (lf-parent ont::awful-val)
     (SEM (f::gradability +) (f::orientation ont::less) (f::intensity ont::hi))
     (TEMPL adj-purpose-TEMPL (XP (% W::PP (W::PTYPE W::FOR))))
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("awful%5:00:00:bad:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-NP-OMIT))
     (EXAMPLE "a drug suitable for cancer")
     (SEM (f::gradability +) (f::orientation ont::less) (f::intensity ont::hi))
     (lf-parent ont::awful-val)
     (TEMPL adj-purpose-implicit-XP-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("awful%5:00:00:bad:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-NP-OMIT))
     (EXAMPLE "a solution good for him")
     (lf-parent ont::awful-val)
     (SEM (f::gradability +) (f::orientation ont::less) (f::intensity ont::hi))
     (TEMPL adj-affected-XP-templ)
     )
    )
   )
))

