;;;;
;;;; w::cure
;;;;

(define-words :pos W::n
 :words (
  (w::cure
  (senses
   ((LF-PARENT ont::medication)
    (TEMPL count-PRED-TEMPL)
    (meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
    (example "take a cure")
    )
   )
)
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::cure
   (SENSES
    ((EXAMPLE "The doctor cured my arthritis")
     (LF-PARENT ONT::CURE)
     )
    ((EXAMPLE "The doctor cured me of arthritis")
     (LF-PARENT ONT::CURE)
     (TEMPL agent-affected-theme-optional-templ (xp (% w::pp (w::ptype w::of))))
     )
    )
   )
))

