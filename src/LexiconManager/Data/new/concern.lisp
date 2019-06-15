;;;;
;;;; W::concern
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::concern
   (SENSES
    ((meta-data :origin calo :entry-date 20050425 :change-date nil :wn ("concern%1:09:01") :comments projector-purchasing)
     (LF-PARENT ONT::trouble)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::concern
   (SENSES
    ((EXAMPLE "the problem concerns him")
     (LF-PARENT ONT::evoke-worry)
     (meta-data :origin calo :entry-date 20050425 :change-date nil :comments projector-purchasing)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
;   the problem concerns last year's budget
    )
   )
))

