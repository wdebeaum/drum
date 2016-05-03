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
     (LF-PARENT ONT::familiarity-val)
     (TEMPL central-adj-TEMPL)
     )
    ((meta-data :origin cernl :entry-date 20100501 :change-date nil :comments nil)
     (EXAMPLE "a person well-known to him")
     (LF-PARENT ONT::familiarity-val)
     (TEMPL adj-affected-xp-TEMPL (XP (% W::PP (W::PTYPE W::to))))
     )
    )
   )
))

