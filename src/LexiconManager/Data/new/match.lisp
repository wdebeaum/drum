;;;;
;;;; W::match
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   (W::match
   (SENSES
    ((EXAMPLE "it doesn't match your specifications")
     (LF-PARENT ONT::object-compare)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL neutral-neutral-templ)
     (meta-data :origin calo :entry-date 20031206 :change-date nil :comments calo-y1script)
     )
    )
   )
))

