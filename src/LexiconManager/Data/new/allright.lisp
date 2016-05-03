;;;;
;;;; W::ALLRIGHT
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::ALLRIGHT
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :comlex (EXTRAP-ADJ-FOR-TO-INF-NP-OMIT))
     (example "a good book")
     (LF-PARENT ONT::ACCEPTABILITY-VAL)
     (SEM (f::orientation ont::more) (f::intensity ont::lo))
     (TEMPL central-adj-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :comlex (EXTRAP-ADJ-FOR-TO-INF-NP-OMIT))
     (example "a wall good for climbing")
     (LF-PARENT ONT::ACCEPTABILITY-VAL)
     (SEM (f::orientation ont::more) (f::intensity ont::lo))
     (TEMPL adj-purpose-TEMPL (XP (% W::PP (W::PTYPE W::FOR))))
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :comlex (EXTRAP-ADJ-FOR-TO-INF-NP-OMIT))
     (EXAMPLE "a drug suitable for cancer")
     (LF-PARENT ONT::ACCEPTABILITY-VAL)
     (SEM (f::orientation ont::more) (f::intensity ont::lo))
     ;; this is a sense that allows for implicit/indirect senses of "for"
     ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
     ;; the adj-purpose-implicit-templ is for indirect purposes, such as "this is good for cancer" where one has to infer that the actual use is in the treatment action
     (TEMPL adj-purpose-implicit-XP-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :comlex (EXTRAP-ADJ-FOR-TO-INF-NP-OMIT))
     (EXAMPLE "a solution good for him")
     (LF-PARENT ONT::ACCEPTABILITY-VAL)
     (SEM (f::orientation ont::more) (f::intensity ont::lo))
     ;; this is another indirect sense of "for"
     ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
     ;; the adj-affected-templ is for cases when adjective describes how people are affected, such as "this is good for him" where one has to infer the exact action/result it is good for
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

