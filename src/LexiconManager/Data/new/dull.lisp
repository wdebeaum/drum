;;;;
;;;; W::dull
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::dull
   (wordfeats (W::morph (:FORMS (-er -LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("interesting%3:00:00")  :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (example "a dull book")
     (LF-PARENT ONT::boring)
     (TEMPL central-adj-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("interesting%3:00:00")  :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (EXAMPLE "it is dull for him")
     (LF-PARENT ONT::boring)
     ;; this is another indirect sense of "for"
     ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
     ;; the adj-affected-templ is for cases when adjective describes how people are affected, such as "this is good for him" where one has to infer the exact action/result it is good for
     (TEMPL adj-affected-XP-templ)
     )
    ((meta-data :origin cardiac :entry-date 20080509 :change-date 20090731 :comments LM-vocab)
     (LF-PARENT ONT::weak)
     (sem (f::gradability +) (f::intensity ont::lo) (f::orientation ont::less))
     (example "a dull pain")
     )
    )   
   )
))

