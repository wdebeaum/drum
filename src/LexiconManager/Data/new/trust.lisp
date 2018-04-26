;;;;
;;;; W::trust
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::trust
   (SENSES
    ((LF-PARENT ONT::confidence-scale)
      (meta-data :origin task-learning :entry-date 20080206 :change-date nil :wn ("confidence%1:12:00") :comments nil)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::trust
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (LF-PARENT ONT::TRUST)
     (SEM (F::Aspect F::Indiv-level))
     (TEMPL neutral-neutral-xp-templ)
     )
    )
   )
))

