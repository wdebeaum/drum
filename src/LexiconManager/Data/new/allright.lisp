
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::ALLRIGHT
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :comlex (EXTRAP-ADJ-FOR-TO-INF-NP-OMIT))
     (example "a good book")
     (lf-parent ont::good)
     (SEM (f::orientation ont::more) (f::intensity ont::lo))
     (TEMPL central-adj-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :comlex (EXTRAP-ADJ-FOR-TO-INF-NP-OMIT))
     (example "a wall good for climbing")
     (lf-parent ont::good)
     (SEM (f::orientation ont::more) (f::intensity ont::lo))
     (TEMPL adj-purpose-TEMPL (XP (% W::PP (W::PTYPE W::FOR))))
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :comlex (EXTRAP-ADJ-FOR-TO-INF-NP-OMIT))
     (EXAMPLE "a drug suitable for cancer")
     (lf-parent ont::good)
     (SEM (f::orientation ont::more) (f::intensity ont::lo))
     (TEMPL adj-purpose-implicit-XP-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :comlex (EXTRAP-ADJ-FOR-TO-INF-NP-OMIT))
     (EXAMPLE "a solution good for him")
     (lf-parent ont::good)
     (SEM (f::orientation ont::more) (f::intensity ont::lo))
     (TEMPL adj-affected-XP-templ)
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
    (W::allright
   (SENSES
    ((LF-PARENT ONT::manner)
     (TEMPL PRED-S-POST-TEMPL)
     (example "are you breathing allright today")
     (meta-data :origin chf :entry-date 20070904 :change-date nil :comments nil :wn nil)
     )
    )
   )
))

