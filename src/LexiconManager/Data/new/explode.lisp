;;;;
;;;; w::explode
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   (w::explode
    (SENSES
     ((meta-data :origin step :entry-date 20080705 :change-date 20090504 :comments step5)
      (EXAMPLE "the bomb exploded")
      (LF-PARENT ONT::explode)
      (TEMPL THEME-TEMPL)
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      )
     )
    )
))

