;;;;
;;;; W::divide
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::divide
   (wordfeats (W::morph (:forms (-vb) :nom w::division)))
   (SENSES
     ((meta-data :origin monroe :entry-date 20031219 :change-date 20090522 :comments s11 :vn ("separate-23.1-1"))
      (LF-PARENT ont::separation)
      (example "divide the crews into/in teams" "divide up the crews")
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      (TEMPL AGENT-AFFECTED-RESULT-XP-NP-TEMPL (xp (% W::PP (W::ptype W::into))))
      )
     ((meta-data :origin ptb :entry-date 20100603 :change-date 20090522 :comments s11 :vn ("separate-23.1-1"))
      (LF-PARENT ont::separation)
      (example "critical opinion is divided")
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
      )
     (
      (LF-PARENT ont::SPATIAL-DIVIDE) ;spatial-separate) 
      (EXAMPLE "the fence divides the field")
      (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
      )

     #|((LF-PARENT ont::SPATIAL-DIVIDE) ;spatial-separate)      ;; maybe add if we add NEUTRAL2 to the type, unless there's a better interpretation
      (EXAMPLE "the fence divides the field from the road")
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL (xp (% W::PP (W::ptype (? t W::from)))))
      )|#

     ((meta-data :origin lam :entry-date 20050420 :change-date 20090522 :comments lam-initial)
      (lf-parent ont::calc-divide)
      (example "divide by 5")
      (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% w::pp (w::ptype w::by))))
      (preference .98) ; prefer separation sense 
      )
     ((meta-data :origin "wordnet-3.0" :entry-date 20090522 :change-date nil :comments nil)
      (lf-parent ONT::calc-divide)
      (example "divide 7 into 21")
      (TEMPL AGENT-FORMAL1-FORMAL-XP-TEMPL)
      (preference .98) ; prefer separation sense 
      )
    )
   )
))

#|
(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::divide w::up)
   (SENSES
     ((meta-data :origin monroe :entry-date 20031219 :change-date nil :comments s11)
      (LF-PARENT ont::separation)
      (example "divide up the crews")
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      )
    )
   )
))
|#
