;;;;
;;;; w::lack
;;;;

(define-words :pos W::n
 :words (
  ((w::lack w::of w::appetite)
  (senses
   ((LF-PARENT ONT::physical-symptom)
    (TEMPL mass-pred-TEMPL)
    (syntax (W::morph (:forms (-none))))
    )
   )
)
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::lack
   (wordfeats (W::morph (:forms (-vb) :nom W::lack)))
   (SENSES
    ((EXAMPLE "He lacked enthusiasm")
     (LF-PARENT ONT::lacking)
      (TEMPL neutral-neutral-templ)
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     )
    )
   )
))

