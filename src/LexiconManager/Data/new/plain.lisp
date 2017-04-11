;;;;
;;;; W::PLAIN
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
;   )
  (W::PLAIN
   (wordfeats (W::morph (:FORMS (-LY -ER))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("plain%5:00:00:obvious:00"))
     (LF-PARENT ONT::CLEAR)
     (SEM (F::GRADABILITY F::+))
     )
    ((LF-PARENT ont::not-fancy-val)
     (example "plain food")
     (meta-data :origin adjective-reorganization :entry-date 20170317 :change-date nil :comments nil)
     )
    ((LF-PARENT ont::unadorned)
     (example "plain white walls")
     (meta-data :origin adjective-reorganization :entry-date 20170317 :change-date nil :comments nil)
     )
    )
   )
))

