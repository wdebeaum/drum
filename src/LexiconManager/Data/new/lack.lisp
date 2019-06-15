;;;;
;;;; w::lack
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::lack
   (wordfeats (W::morph (:forms (-vb) :nom W::lack)))
   (SENSES
    ((EXAMPLE "He lacked enthusiasm")
     (LF-PARENT ONT::lacking)
      (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     )
    )
   )
))


#|
(define-words :pos W::n
 :words (
  ((w::lack w::of w::appetite)
  (senses
   ((meta-data :wn ("anorexia%1:26:00"))
    (LF-PARENT ONT::medical-condition)
    (TEMPL mass-pred-TEMPL)
    (syntax (W::morph (:forms (-none))))
    )
   )
)
))
|#


