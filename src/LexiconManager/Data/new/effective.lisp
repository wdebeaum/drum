;;;;
;;;; W::EFFECTIVE
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::EFFECTIVE
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("effective%3:00:00" "effective%5:00:00:competent:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (LF-PARENT ONT::effective-VAL)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("effective%3:00:00" "effective%5:00:00:competent:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (example "a drug effective for treating cancer")
     (LF-PARENT ONT::effective-VAL)
     (TEMPL adj-purpose-TEMPL (XP (% W::PP (W::PTYPE W::FOR))))     
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("effective%3:00:00" "effective%5:00:00:competent:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (LF-PARENT ONT::effective-VAL)
     (example "a drug effective for leukemia")
     ;; this is a sense that allows for implicit/indirect senses of "for"
     ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
     ;; the adj-purpose-implicit-templ is for indirect purposes, such as "this is good for cancer" where one has to infer that the actual use is in the treatment action
     (TEMPL adj-purpose-implicit-XP-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("effective%3:00:00" "effective%5:00:00:competent:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (LF-PARENT ONT::effective-VAL)
     (example "a drug effective for him")
     ;; this is another indirect sense of "for"
     ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
     ;; the adj-affected-templ is for cases when adjective describes how people are affected, such as "this is good for him" where one has to infer the exact action/result it is good for
     (TEMPL adj-affected-XP-templ)
     )    
    )
   )
))

