;;;;
;;;; W::release
;;;;

#||(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
;   )
  (W::release
   (SENSES
    ((meta-data :origin caloy2 :entry-date 20050404 :change-date nil :wn ("release%1:10:00") :comments projector-purchasing)
     (LF-PARENT ONT::gathering-event)
     (example "a press release")
     )
    )
   )
))||#

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::release
   (SENSES
    ((LF-PARENT ont::version)
     (example "a new software release")
     (meta-data :origin integrated-learning :entry-date 20050817 :change-date nil :wn ("release%1:06:00") :comments nil)
     (templ other-reln-templ)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (w::release
   (wordfeats (W::morph (:forms (-vb) :nom W::release)))
   (senses
    ((LF-PARENT ont::releasing)
     (meta-data :origin step :entry-date 20060630 :change-date nil :comments nil :vn ("free-78-1"))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "the curtain releases toxic fumes")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

