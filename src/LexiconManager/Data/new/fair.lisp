;;;;
;;;; W::FAIR
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::FAIR
   (wordfeats (W::morph (:FORMS (-er -ly))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :wn ("fair%5:00:00:moderate:00") :comlex (ADJ-PP-FOR))
     (example "a good book")
     (LF-PARENT ONT::good-VAL)
     (SEM (f::gradability +) (f::orientation ont::less) (f::intensity ont::lo))
     (TEMPL central-adj-templ)
     )
;;;    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :wn ("fair%5:00:00:moderate:00") :comlex (EXTRAP-ADJ-FOR-TO-INF))
;;;     (example "a wall good for climbing")
;;;     (LF-PARENT ONT::ACCEPTABILITY-VAL)
;;;     (TEMPL adj-purpose-TEMPL)
;;;     )
;;;    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :wn ("fair%5:00:00:moderate:00") :comlex (EXTRAP-ADJ-FOR-TO-INF))
;;;     (EXAMPLE "a drug suitable for cancer")
;;;     (LF-PARENT ONT::ACCEPTABILITY-VAL)
;;;     ;; this is a sense that allows for implicit/indirect senses of "for"
;;;     ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
;;;     ;; the adj-purpose-implicit-templ is for indirect purposes, such as "this is good for cancer" where one has to infer that the actual use is in the treatment action
;;;     (TEMPL adj-purpose-implicit-XP-templ)
;;;     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :wn ("fair%5:00:00:moderate:00") :comlex (ADJ-PP-FOR))
     (EXAMPLE "a solution good for him")
     (LF-PARENT ONT::good-VAL)
     (SEM (f::gradability +) (f::orientation ont::less) (f::intensity ont::lo))
     ;; this is another indirect sense of "for"
     ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
     ;; the adj-affected-templ is for cases when adjective describes how people are affected, such as "this is good for him" where one has to infer the exact action/result it is good for
     (TEMPL adj-affected-XP-templ)
     )
    ((meta-data :origin plow :entry-date 20060712  :change-date 20090819 :wn ("fair%5:00:00:clear:03") :comments pq)
     (example "fair weather predicted for tomorrow")
     (LF-PARENT ONT::CLEAR-WEATHER)
     )
    )
   )
))

