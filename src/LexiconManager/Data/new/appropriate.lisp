;;;;
;;;; W::appropriate
;;;;

(define-words :pos W::v 
 :words (
  (W::appropriate
     (wordfeats (W::morph (:forms (-vb) :nom W::appropriation)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090430 :comments nil :vn ("obtain-13.5.2") :wn ("appropriate%2:31:00" "appropriate%2:40:00"))
     (LF-PARENT ONT::appropriate)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::appropriate
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :wn ("appropriate%3:00:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-NP-OMIT))
     (example "a good book")
     (LF-PARENT ONT::appropriateness-VAL)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::med))
     (TEMPL central-adj-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :wn ("appropriate%3:00:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-NP-OMIT))    
     (example "a wall good for climbing")
     (LF-PARENT ONT::APPROPRIATENESS-VAL)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::med))
     (TEMPL adj-purpose-TEMPL)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :wn ("appropriate%3:00:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-NP-OMIT))    
     (EXAMPLE "a drug suitable for cancer")
     (LF-PARENT ONT::APPROPRIATENESS-VAL)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::med))
     ;; this is a sense that allows for implicit/indirect senses of "for"
     ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
     ;; the adj-purpose-implicit-templ is for indirect purposes, such as "this is good for cancer" where one has to infer that the actual use is in the treatment action
     (TEMPL adj-purpose-implicit-XP-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :wn ("appropriate%3:00:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-NP-OMIT))    
     (EXAMPLE "a solution good for him")
     (LF-PARENT ONT::APPROPRIATENESS-VAL)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::med))
     ;; this is another indirect sense of "for"
     ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
     ;; the adj-affected-templ is for cases when adjective describes how people are affected, such as "this is good for him" where one has to infer the exact action/result it is good for
     (TEMPL adj-affected-XP-templ)
     )
    )
   )
))

