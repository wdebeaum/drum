;;;;
;;;; w::likely
;;;; 

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::likely
   (SENSES
    ((meta-data :origin calo :entry-date 20031223 :change-date nil :wn ("likely%5:00:00:prospective:00") :comments html-purchasing-corpus)
     (EXAMPLE "He is a likely candidate")
     (lf-parent ont::likely-val)
     (SEM (F::GRADABILITY +))
     (TEMPL central-adj-xp-TEMPL (XP (% W::cp (W::ctype W::s-to))))
     )
    #|
    ((meta-data :origin adjective-reorganization :entry-date 20170322 :change-date nil :comments nil)
     (EXAMPLE "It's a likely excuse")
     (lf-parent ont::nonactual)
    )
    |#
   )
)))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
   (W::likely
   (SENSES
    ((LF-PARENT ONT::likely-val)
     (example "he (most) likely left")
     (meta-data :origin cardiac :entry-date 20090427)
     (TEMPL PRED-S-VP-TEMPL)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::LIKELY
   (SENSES
    ((LF-PARENT ONT::QUALIFICATION)
     (example "it is likely broken")
     (TEMPL PRED-S-VP-TEMPL)
     (PREFERENCE 0.96) ;; prefer the adjective if possible
     )
    )
   )
))

