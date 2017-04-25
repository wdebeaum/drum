;;;;
;;;; W::brilliant
;;;;

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
     ;; this is a sense that allows for implicit/indirect senses of "for"
     ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
     ;; the adj-purpose-implicit-templ is for indirect purposes, such as "this is good for cancer" where one has to infer that the actual use is in the treatment action
     (TEMPL adj-purpose-implicit-XP-templ)
     )
    ((meta-data :origin lam :entry-date 20050422 :change-date 20061106 :wn ("brilliant%5:00:00:superior:00") :comments lam-initial :comlex (ADJ-PP-FOR))     
     (EXAMPLE "a solution good for him")
     (lf-parent ont::great-val)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::hi))
     ;; this is another indirect sense of "for"
     ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
     ;; the adj-affected-templ is for cases when adjective describes how people are affected, such as "this is good for him" where one has to infer the exact action/result it is good for
     (TEMPL adj-affected-XP-templ)
     )
    )
   )
))

