;;;;
;;;; W::unfamiliar
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::unfamiliar
   (SENSES
    ((meta-data :origin cernl :entry-date 20100501 :change-date nil :comments nil)
     (EXAMPLE "an unfamiliar person")
     (LF-PARENT ONT::unfamiliarity-val)
     (TEMPL central-adj-TEMPL)
     )
    ((meta-data :origin cernl :entry-date 20100501 :change-date nil :comments nil)
     (EXAMPLE "that's unfamiliar to him")
     (LF-PARENT ONT::unfamiliarity-val)
     (templ adj-affected-XP-templ  (xp (% w::pp (w::ptype w::to))))
     )
    ((meta-data :origin ptb :entry-date 20100501 :change-date nil :comments nil)
     (EXAMPLE "he is unfamiliar with it")
     (LF-PARENT ONT::unfamiliarity-val)
     (templ adj-affected-stimulus-xp-templ  (xp (% w::pp (w::ptype w::with))))
     )
    )
   )
))

