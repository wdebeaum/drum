;;;;
;;;; W::LATE
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::LATE
   (wordfeats (W::MORPH (:FORMS (-ER -LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("late%3:00:00"))
     (LF-PARENT ONT::SCHEDULED-TIME-MODIFIER)
     )
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
;   )
  (W::LATE
   (SENSES
    ((LF-PARENT ONT::event-time-rel)
     (templ pred-s-post-templ)
     )
    )
   )
))

