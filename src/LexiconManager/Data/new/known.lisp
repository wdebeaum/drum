;;;;
;;;; W::known
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   ;; later- derive from verb
   (W::known
   (SENSES
    ((meta-data :origin cernl :entry-date 20100501 :change-date nil :comments nil)
     (EXAMPLE "a well-known person")
     (lf-parent ont::familiar-val)
     (TEMPL central-adj-TEMPL)
     )
    ((meta-data :origin cernl :entry-date 20100501 :change-date nil :comments nil)
     (EXAMPLE "a person well-known to him")
     (lf-parent ont::familiar-val)
     (TEMPL adj-affected-xp-TEMPL (XP (% W::PP (W::PTYPE W::to))))
     )
    )
   )
))

