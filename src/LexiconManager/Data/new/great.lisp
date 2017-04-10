
(define-words :pos W::n
 :words (
  ((W::GREAT W::NORTHERN W::BEAN)
  (senses
	   ((LF-PARENT ONT::BEANS-PEAS)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::GREAT
   (wordfeats (W::MORPH (:FORMS (-ER -LY))))   
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("great%5:00:00:good:01") :comlex (EXTRAP-ADJ-FOR-TO-INF-RS))
     (example "a good book")
     (lf-parent ont::great-val)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::hi))
     (TEMPL central-adj-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("great%5:00:00:good:01") :comlex (EXTRAP-ADJ-FOR-TO-INF-RS))
     (example "a wall good for climbing")
     (lf-parent ont::great-val)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::hi))
     (TEMPL adj-purpose-TEMPL)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("great%5:00:00:good:01") :comlex (EXTRAP-ADJ-FOR-TO-INF-RS))
     (EXAMPLE "a drug suitable for cancer")
     (lf-parent ont::great-val)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::hi))
     (TEMPL adj-purpose-implicit-XP-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("great%5:00:00:good:01") :comlex (EXTRAP-ADJ-FOR-TO-INF-RS))
     (EXAMPLE "a solution good for him")
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::hi))
     (lf-parent ont::great-val)
     (TEMPL adj-affected-XP-templ)
     )
    )
   )
))

