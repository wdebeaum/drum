;;;;
;;;; W::EASY
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::EASY
   (wordfeats (W::morph (:FORMS (-ER))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090821 :comments nil :wn ("easy%3:00:01"))
     (EXAMPLE "that's easy [for him]")
     (LF-PARENT ONT::easy)
     (TEMPL adj-content-affected-optional-xp-templ)
     )   
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090821 :comments nil :wn ("easy%3:00:01"))
     (EXAMPLE "it's easy to do")
     (LF-PARENT ONT::easy)
     (TEMPL adj-expletive-content-xp-templ (XP (% W::CP (W::CTYPE W::s-to))))
     )
    )
   )
))


(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::easy
   (SENSES
    ((meta-data :origin asma :entry-date 20110815 :change-date nil :comments nil)
     (LF-PARENT ONT::easy)
     (example "breathing easy")
     (TEMPL PRED-S-VP-TEMPL)
     )
    )
   )
))


