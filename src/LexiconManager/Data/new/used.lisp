;;;;
;;;; W::used
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::used
   (wordfeats (W::morph (:forms NIL)) (W::AGR ?agr) (W::vform W::past))
    (SENSES
     ((LF-PARENT ont::habitual)
      (example "Abrams used to hire Browne")
      (TEMPL AGENT-FORMAL-SUBJCONTROL-TEMPL (xp (% W::cp (W::ctype W::s-to))))
      (meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil)
      )
     #|
     ((LF-PARENT ont::habitual)
      (example "the meetings used to drive them crazy")
      (TEMPL AGENT-FORMAL-SUBJCONTROL-TEMPL (xp (% W::cp (W::ctype W::s-to))))
      (meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil)
      )
     |#
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
     (SEM (F::GRADABILITY +))
     (TEMPL adj-experiencer-theme-req-templ (xp (% w::np)))
     )
    )
   )
))

