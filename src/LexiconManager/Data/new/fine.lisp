;;;;
;;;; W::fine
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::fine
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090508 :comments nil :vn ("judgement-33") :wn ("fine%2:40:00" "fine%2:41:00"))
     (LF-PARENT ont::punish)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::FINE
   (wordfeats (W::MORPH (:FORMS (-er -LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("fine%5:00:00:satisfactory:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (LF-PARENT ONT::good)
     (SEM (f::gradability +) (f::orientation F::pos) (f::intensity ont::lo))
     )    
   ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("fine%5:00:00:satisfactory:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (example "a wall good for climbing")
     (LF-PARENT ONT::good)
     (TEMPL adj-purpose-TEMPL (XP (% W::PP (W::PTYPE W::FOR))))
     (SEM (f::gradability +) (f::orientation F::pos) (f::intensity ont::lo))
     )
;    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("fine%5:00:00:satisfactory:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
;     (EXAMPLE "a drug suitable for cancer")
;     (LF-PARENT ONT::good)
;     (SEM (f::gradability +) (f::orientation F::pos) (f::intensity ont::lo))
;     ;; this is a sense that allows for implicit/indirect senses of "for"
;     ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
;     ;; the adj-purpose-implicit-templ is for indirect purposes, such as "this is good for cancer" where one has to infer that the actual use is in the treatment action
;     (TEMPL adj-purpose-implicit-XP-templ)
;     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("fine%5:00:00:satisfactory:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-OMIT))
     (EXAMPLE "a solution good for him")
     (LF-PARENT ONT::good)
     (SEM (f::gradability +) (f::orientation F::pos) (f::intensity ont::lo))
     ;; this is another indirect sense of "for"
     ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
     ;; the adj-affected-templ is for cases when adjective describes how people are affected, such as "this is good for him" where one has to infer the exact action/result it is good for
     (TEMPL adj-affected-XP-templ)
     )
    )
   )
))

