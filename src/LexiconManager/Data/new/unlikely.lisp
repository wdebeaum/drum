;;;;
;;;; W::unlikely
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::unlikely
   (SENSES
    ((meta-data :origin calo :entry-date 20031223 :change-date nil :wn ("unlikely%5:00:00:implausible:00") :comments html-purchasing-corpus)
     (EXAMPLE "He is unlikely to do this")
     (LF-PARENT ONT::expectation-val)
     (SEM (F::GRADABILITY F::+))
     (TEMPL central-ADJ-xp-TEMPL (XP (% W::cp (W::ctype W::s-to))))
     )
    ;; it is unlikely that...
    )
   )
))

