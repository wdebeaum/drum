;;;;
;;;; W::WORTHWHILE
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::WORTHWHILE
   (SENSES
    ((meta-data :origin calo :entry-date 20040915 :change-date 20061106 :wn ("worthwhile%5:00:00:worthy:00") :comments caloy2  :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (example "a good book")
     (lf-parent ont::worthy-val)
     (TEMPL central-adj-templ)
     )
    ((meta-data :origin calo :entry-date 20040915 :change-date 20061106 :wn ("worthwhile%5:00:00:worthy:00") :comments caloy2  :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (example "a wall good for climbing")
     (lf-parent ont::worthy-val)
     (TEMPL adj-purpose-TEMPL (XP (% W::PP (W::PTYPE W::FOR))))
     )
    ((meta-data :origin calo :entry-date 20040915 :change-date 20061106 :wn ("worthwhile%5:00:00:worthy:00") :comments caloy2  :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (EXAMPLE "a drug suitable for cancer")
     (lf-parent ont::worthy-val)
     ;; this is a sense that allows for implicit/indirect senses of "for"
     ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
     ;; the adj-purpose-implicit-templ is for indirect purposes, such as "this is good for cancer" where one has to infer that the actual use is in the treatment action
     (TEMPL adj-purpose-implicit-XP-templ)
     )
    ((meta-data :origin calo :entry-date 20040915 :change-date 20061106 :wn ("worthwhile%5:00:00:worthy:00") :comments caloy2  :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (EXAMPLE "a solution good for him")
     (lf-parent ont::worthy-val)
     ;; this is another indirect sense of "for"
     ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
     ;; the adj-affected-templ is for cases when adjective describes how people are affected, such as "this is good for him" where one has to infer the exact action/result it is good for
     (TEMPL adj-affected-XP-templ)
     )    
    )
   )
))

