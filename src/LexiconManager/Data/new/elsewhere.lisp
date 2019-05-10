;;;;
;;;; W::ELSEWHERE
;;;;

(define-words :pos W::adv :templ PPWORD-ADV-TEMPL
 :tags (:base500)
 :words (
  (W::ELSEWHERE
   (SENSES
    ((LF-PARENT ONT::AT-LOC)
     (SYNTAX (W::IMPRO-CLASS ONT::LOCATION))
     (meta-data :origin step :entry-date 20080724 :change-date nil :comments step6) 
     )
    )
   )
))

(define-words :pos W::n :templ PPWORD-N-TEMPL
 :tags (:base500)
 :words (
  (W::ELSEWHERE
   (SENSES
    ((LF-PARENT ONT::LOCATION)
     (PREFERENCE 0.97)
     )
    )
   )
))
