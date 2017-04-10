
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::INTERESTING
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :wn ("interesting%3:00:00")  :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (example "a good book")
     (lf-parent ont::interesting-val)
     (TEMPL central-adj-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :wn ("interesting%3:00:00")  :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (example "a wall good for climbing")
     (lf-parent ont::interesting-val)
     (TEMPL adj-purpose-TEMPL (XP (% W::PP (W::PTYPE W::FOR))))
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :wn ("interesting%3:00:00")  :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (EXAMPLE "a drug suitable for cancer")
     (lf-parent ont::interesting-val)
     (TEMPL adj-purpose-implicit-XP-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :wn ("interesting%3:00:00")  :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (EXAMPLE "a solution good for him")
     (lf-parent ont::interesting-val)
     (TEMPL adj-affected-XP-templ)
     )
    )   
   )
))

