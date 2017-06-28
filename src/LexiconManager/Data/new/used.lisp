;;;;
;;;; W::used
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::used
   (wordfeats (W::morph (:forms NIL)) (W::AGR ?agr) (W::vform W::past))
    (SENSES
     ((LF-PARENT ont::habitual)
      (example "Abrams used to hire Browne")
      (TEMPL AGENT-effect-SUBJCONTROL-TEMPL (xp (% W::cp (W::ctype W::s-to))))
      (meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil)
      )
     ((LF-PARENT ont::habitual)
      (example "the meetings used to drive them crazy")
      (TEMPL agent-effect-subjcontrol-templ (xp (% W::cp (W::ctype W::s-to))))
      (meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil)
      )
     )
    )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ((W::used w::to)
   (SENSES
    ((meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil :wn nil)
     (EXAMPLE "He is used to that")
     (LF-PARENT ONT::habituated-val)
     (SEM (F::GRADABILITY F::+))
     (TEMPL adj-experiencer-theme-req-templ (xp (% w::np)))
     )
    )
   )
))

