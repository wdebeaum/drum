;;;;
;;;; W::LOOP
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::LOOP
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("loop%1:25:00"))
     (LF-PARENT ONT::graphic-symbol)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::LOOP
   (SENSES
    ;;;; Loop up to avon
    ((LF-PARENT ONT::circular-move)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL THEME-TEMPL)
     )
    )
   )
))

