;;;;
;;;; W::dehydrated
;;;;

(define-words :pos W::adj :templ ADJ-EXPERIENCER-TEMPL
 :words (
   (W::dehydrated
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("dehydrated%5:00:00:preserved:02"))
     (lf-parent ont::medical-dehydration-val)
     (templ central-adj-templ)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
    (W::dehydrated
   (SENSES
    ((meta-data :origin cause-result-relations :entry-date 20180809 :change-date nil :comments nil)
     (lf-parent ont::dehydrated-val)
     (example "raisins are dehydrated grapes")
     )
    )
   )
))