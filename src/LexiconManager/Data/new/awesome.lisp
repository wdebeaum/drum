;;;;
;;;; W::awesome
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::awesome
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :wn ("awesome%5:00:00:impressive:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (example "a good book")
     (lf-parent ont::great-val)
     (SEM (f::gradability +) (f::orientation F::pos) (f::intensity ont::hi))
     (TEMPL central-adj-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :wn ("awesome%5:00:00:impressive:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (example "a wall good for climbing")
     (lf-parent ont::great-val)
     (SEM (f::gradability +) (f::orientation F::pos) (f::intensity ont::hi))
     (TEMPL adj-purpose-TEMPL)
     )
;    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :wn ("awesome%5:00:00:impressive:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
;     (EXAMPLE "a drug suitable for cancer")
;     (lf-parent ont::great-val)
;     (SEM (f::gradability +) (f::orientation F::neg) (f::intensity ont::hi))
;     ;; this is a sense that allows for implicit/indirect senses of "for"
;     ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
;     ;; the adj-purpose-implicit-templ is for indirect purposes, such as "this is good for cancer" where one has to infer that the actual use is in the treatment action
;     (TEMPL adj-purpose-implicit-XP-templ)
;     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :wn ("awesome%5:00:00:impressive:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (EXAMPLE "a solution good for him")
     (lf-parent ont::great-val)
     (SEM (f::gradability +) (f::orientation F::pos) (f::intensity ont::hi))
     ;; this is another indirect sense of "for"
     ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
     ;; the adj-affected-templ is for cases when adjective describes how people are affected, such as "this is good for him" where one has to infer the exact action/result it is good for
     (TEMPL adj-affected-XP-templ)
     )
    )
   )
))

