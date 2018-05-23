;;;;
;;;; W::noon
;;;;

(define-words :pos W::pro :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
;; at noon, midnight
;; why are these pronouns? 
  (W::noon
   (SENSES
    ((LF-PARENT ONT::recurring-time-of-day)
     (SEM (F::time-function F::day-point))
     )
    )
   )
))

