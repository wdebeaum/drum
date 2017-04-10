
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::familiar
   (SENSES
    ((meta-data :origin cernl :entry-date 20100501 :change-date nil :comments nil)
     (EXAMPLE "a familiar person")
     (lf-parent ont::familiar-val)
     (TEMPL central-adj-TEMPL)
     )
    ((meta-data :origin cernl :entry-date 20100501 :change-date nil :comments nil)
     (EXAMPLE "that's familiar to him")
     (lf-parent ont::familiar-val)
     (templ adj-affected-XP-templ  (xp (% w::pp (w::ptype w::to))))
     )
    ((meta-data :origin ptb :entry-date 20100501 :change-date nil :comments nil)
     (EXAMPLE "he is familiar with it")
     (lf-parent ont::familiar-val)
     (templ adj-affected-stimulus-xp-templ  (xp (% w::pp (w::ptype w::with))))
     )
    )
   )
))

