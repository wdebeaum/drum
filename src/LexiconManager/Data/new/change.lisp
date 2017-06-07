;;;;
;;;; W::change
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
  (W::change
     (wordfeats (W::morph (:forms (-vb) :nom w::change :nomobjpreps (w::in w::of))))
   (SENSES
    ((LF-PARENT ONT::change)
     (example "change the plan")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
   #||   done by general principles now with the RESULT construction
    ((LF-PARENT ONT::change)
     (example "change the color to grey")
     (TEMPL AGENT-affected-RESULT-TEMPL (xp (% w::advbl (W::lf (% ?p (w::class (? x ont::resulting-object ont::resulting-state)))))))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin fruitcarts :entry-date 20050331 :change-date nil :comments fruitcarts-11-1 :vn ("turn-26.6-1"))   
     )||#
    ((meta-data :origin step :entry-date 20080626 :change-date nil :comments nil)
      (LF-PARENT ONT::change)
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      (example "it changed in color / with time" "the room changed")
      (templ affected-theme-xp-optional-templ  (xp (% W::PP (W::ptype (? pt w::in W::with)))))
      )
   ))
))

#|
(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  ((W::change (w::over)) ; not sure this works with morphology -- wdebeaum
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("convert-26.6.2-1"))
     (LF-PARENT ONT::replacement)
 ; like switch
     )
    )
   )
))
|#
