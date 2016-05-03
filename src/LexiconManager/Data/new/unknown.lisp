;;;;
;;;; W::unknown
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::unknown
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("unknown%3:00:00"))
     (LF-PARENT ONT::unfamiliarity-VAL)
     )
      ((meta-data :origin cernl :entry-date 20100501 :change-date nil :comments nil)
     (EXAMPLE "a person unknown to him")
     (LF-PARENT ONT::unfamiliarity-val)
     (TEMPL adj-affected-xp-TEMPL (XP (% W::PP (W::PTYPE W::to))))
     )
    )
   )
))

