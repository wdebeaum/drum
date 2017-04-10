
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::brilliant
   (wordfeats (W::MORPH (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin lam :entry-date 20050422 :change-date 20061106 :wn ("brilliant%5:00:00:superior:00") :comments lam-initial :comlex (EXTRAP-ADJ-FOR-TO-INF-RS))
     (example "a good book")
     (lf-parent ont::great-val)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::hi))
     (TEMPL central-adj-templ)
     )
    ((meta-data :origin lam :entry-date 20050422 :change-date 20061106 :wn ("brilliant%5:00:00:superior:00") :comments lam-initial :comlex (EXTRAP-ADJ-FOR-TO-INF-RS))     
     (example "a wall good for climbing")
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::hi))
     (lf-parent ont::great-val)
     (TEMPL adj-purpose-TEMPL)
     )
    ((meta-data :origin lam :entry-date 20050422 :change-date 20061106 :wn ("brilliant%5:00:00:superior:00") :comments lam-initial :comlex (EXTRAP-ADJ-FOR-TO-INF-RS))     
     (EXAMPLE "a drug suitable for cancer")
     (lf-parent ont::great-val)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::hi))
     (TEMPL adj-purpose-implicit-XP-templ)
     )
    ((meta-data :origin lam :entry-date 20050422 :change-date 20061106 :wn ("brilliant%5:00:00:superior:00") :comments lam-initial :comlex (ADJ-PP-FOR))     
     (EXAMPLE "a solution good for him")
     (lf-parent ont::great-val)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::hi))
     (TEMPL adj-affected-XP-templ)
     )
    )
   )
))

