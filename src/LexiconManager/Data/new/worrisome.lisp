
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::worrisome
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :wn ("worrisome%3:00:04"))
     (example "a good book")
     (lf-parent ont::distressing-val)
     (SEM (f::gradability +) (f::orientation ont::less) (f::intensity ont::med))
     (TEMPL central-adj-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :wn ("worrisome%3:00:04") :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (example "a wall good for climbing")
     (lf-parent ont::distressing-val)
     (SEM (f::gradability +) (f::orientation ont::less) (f::intensity ont::med))
     (TEMPL adj-purpose-TEMPL)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :wn ("worrisome%3:00:04") :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (EXAMPLE "a drug suitable for cancer")
     (lf-parent ont::distressing-val)
     (SEM (f::gradability +) (f::orientation ont::less) (f::intensity ont::med))
     (TEMPL adj-purpose-implicit-XP-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :wn ("worrisome%3:00:04") :comlex (ADJ-PP-FOR))
     (EXAMPLE "a solution good for him")
     (SEM (f::gradability +) (f::orientation ont::less) (f::intensity ont::med))
     (lf-parent ont::distressing-val)
     (TEMPL adj-affected-XP-templ)
     )        
    )
   )
))

