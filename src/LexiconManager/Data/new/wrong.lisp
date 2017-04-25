;;;;
;;;; w::wrong
;;;; 

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::WRONG
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("wrong%3:00:02"))
     (lf-parent ont::incorrect)
     (templ central-adj-templ)
     (example "that's wrong")
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("wrong%5:00:00:inappropriate:00"))
     (EXAMPLE "that's wrong for him")
     (lf-parent ont::incorrect)
     (TEMPL ADJ-CO-THEME-TEMPL (XP (% W::PP (W::PTYPE W::For))))
     (SYNTAX (W::allow-deleted-comp -))
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
   (W::WRONG
   (SENSES
    ((LF-PARENT ONT::incorrect)
     (example "I might have spelled the name wrong")
     (TEMPL pred-vp-templ)
     (meta-data :origin plow :entry-date 20050922 :change-date nil :comments nil)
     )
    )
   )
))

