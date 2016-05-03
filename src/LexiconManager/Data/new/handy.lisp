;;;;
;;;; W::HANDY
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
;   )
  (W::HANDY
   (wordfeats (W::MORPH (:FORMS (-ER -LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("handy%5:00:00:accessible:00"))
     (EXAMPLE "that's handy [for him]")
     (LF-PARENT ONT::AVAILABILITY-VAL)
     (TEMPL CENTRAL-ADJ-XP-TEMPL (XP (% W::PP (W::PTYPE W::FOR))))
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("handy%5:00:00:accessible:00"))
     (EXAMPLE "that's handy to use")
     (LF-PARENT ONT::AVAILABILITY-VAL)
     (TEMPL CENTRAL-ADJ-XP-TEMPL (XP (% W::CP (W::cTYPE W::s-to))))
     )
    )
   )
))

