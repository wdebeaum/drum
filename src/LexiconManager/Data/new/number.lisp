;;;;
;;;; W::NUMBER
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
;   )
  (W::NUMBER
   (SENSES
    ;; a number is now a quantifier -- all other uses are ont::domain
     ((meta-data :origin plow :entry-date 20060803 :change-date nil :comments nil :wn ("number%1:07:00"))
      ;(LF-PARENT ONT::quantity)
      (LF-PARENT ONT::quantity-abstr)
;      (TEMPL indef-classifier-count-pl-templ)
      ;(templ other-reln-templ)
      (templ OTHER-RELN-subcat-count-TEMPL)
      (example "a certain number of books on the shelf")
      )
    ((LF-PARENT ONT::number)
     (meta-data :origin task-learning :entry-date 20050926 :change-date nil :wn ("number%1:23:00") :comments nil)
     (example "this parameter is a real number")
     ;(templ other-reln-templ)
     (preference 0.96) ;; prefer quantifier use if available
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::NUMBER
   (SENSES
    ((LF-PARENT ONT::categorization)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-FORMAL-XP-TEMPL)
     )
    )
   )
))

