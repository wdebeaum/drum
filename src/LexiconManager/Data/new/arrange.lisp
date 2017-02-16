;;;;
;;;; W::arrange
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::arrange
   (wordfeats (W::morph (:forms (-vb) :nom w::arrangement)))
   (SENSES
    ((meta-data :origin monroe :entry-date 20031219 :change-date 20090515 :comments s11)
     (LF-PARENT ONT::planning)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-effect-objcontrol-templ)
     (example "arrange the meeting [to start at 10am]")
     )
    ((lf-parent ont::arranging)
     (example "arrange them [into groups of three]")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-affected-xp-TEMPL)
     (meta-data :origin calo :entry-date 20040414 :change-date nil :comments calo-y1v4 :vn ("put-9.1") :wn ("arrange%2:35:00"))
     )
    )
   )
))

