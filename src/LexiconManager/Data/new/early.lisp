;;;;
;;;; W::EARLY
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
;   )
  (W::EARLY
   (wordfeats (W::MORPH (:FORMS (-ER))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("early%3:00:00"))
     (LF-PARENT ONT::SCHEDULED-TIME-MODIFIER)
     )
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::EARLY
   (SENSES
    ((LF-PARENT ONT::before)
     (example "he arrived early")
     (templ pred-s-post-templ)
     )
    )
   )
))

