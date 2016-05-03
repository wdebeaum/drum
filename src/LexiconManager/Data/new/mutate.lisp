;;;;
;;;; W::mutate
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::mutate
   (wordfeats (W::morph (:forms (-vb) :nom w::mutation :nomobjpreps (w::in w::of))))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090504 :comments nil :vn ("turn-26.6.1"))
     (LF-PARENT ONT::life-transformation)
     (TEMPL affected-result-optional-templ (xp (% w::pp (w::ptype w::into))))  ;;like translate but pp is into, not to
     )
    )
   )
))

