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

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::concern
   (SENSES
    ((EXAMPLE "the problem concerns him")
     (LF-PARENT ONT::AFFECT-experiencer)
     (meta-data :origin calo :entry-date 20050425 :change-date nil :comments projector-purchasing)
     (TEMPL agent-affected-xp-templ)
     )
;   the problem concerns last year's budget
    )
   )
))

