;;;;
;;;; W::lame
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::lame
   (wordfeats (W::morph (:FORMS (-er -LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "an exceptional book")
     (lf-parent ont::bad)
     (SEM (f::gradability +) (f::orientation ont::less) (f::intensity ont::med))
     (TEMPL central-adj-templ)
     )
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "a wall exceptional for climbing")
     (lf-parent ont::bad)
     (SEM (f::gradability +) (f::orientation ont::less) (f::intensity ont::med))
     (TEMPL adj-purpose-TEMPL)
     )
;    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
;      (EXAMPLE "a drug exceptional for cancer")
;     (lf-parent ont::bad)
;     ;; this is a sense that allows for implicit/indirect senses of "for"
;     ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
;     ;; the adj-purpose-implicit-templ is for indirect purposes, such as "this is good for cancer" where one has to infer that the actual use is in the treatment action
;     (TEMPL adj-purpose-implicit-XP-templ)
;     (SEM (f::gradability +) (f::orientation ont::less) (f::intensity ont::med))
;     )
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (EXAMPLE "a solution good for him")
     (lf-parent ont::bad)
     (SEM (f::gradability +) (f::orientation ont::less) (f::intensity ont::med))
     ;; this is another indirect sense of "for"
     ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
     ;; the adj-affected-templ is for cases when adjective describes how people are affected, such as "this is good for him" where one has to infer the exact action/result it is good for
     (TEMPL adj-affected-XP-templ)
     )
    )
   )
))

