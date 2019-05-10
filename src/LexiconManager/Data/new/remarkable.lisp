;;;;
;;;; W::remarkable
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::remarkable
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "an exceptional book")
     (LF-PARENT ONT::GREAT-VAL)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::hi))
     (TEMPL central-adj-templ)
     )
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "a wall exceptional for climbing")
     (LF-PARENT ONT::GREAT-VAL)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::hi))
     (TEMPL adj-purpose-TEMPL)
     )
#|    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
      (EXAMPLE "a drug exceptional for cancer")
     (LF-PARENT ONT::GREAT-VAL)
     ;; this is a sense that allows for implicit/indirect senses of "for"
     ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
     ;; the adj-purpose-implicit-templ is for indirect purposes, such as "this is good for cancer" where one has to infer that the actual use is in the treatment action
     (TEMPL adj-purpose-implicit-XP-templ)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::hi))
     )|#
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (EXAMPLE "a solution good for him")
     (LF-PARENT ONT::GREAT-VAL)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::hi))
     ;; this is another indirect sense of "for"
     ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
     ;; the adj-affected-templ is for cases when adjective describes how people are affected, such as "this is good for him" where one has to infer the exact action/result it is good for
     (TEMPL adj-affected-XP-templ)
     )
    )
   )
))

(define-words :pos W::adj
 :words (
  (w::remarkable
  (senses
   ((LF-PARENT ONT::EXCEPTIONAL-VAL)
    (TEMPL central-adj-templ)
    (sem (f::gradability +) (f::intensity ont::med) (f::orientation ont::less))
    (meta-data :origin cardiac :entry-date 20090129 :change-date 20090804 :wn ("strange%3:00:00") :comments nil)
    )
   )
)
))

